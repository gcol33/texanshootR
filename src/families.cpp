// Native fitter kernels for the gam and glmm families.
//
// We do not link mgcv / lme4 / Armadillo. Both kernels are small,
// closed-form solves on column-major contiguous storage with a
// hand-rolled Cholesky factorisation. The package fits at most a few
// hundred small models per shoot() call so a textbook decomposition
// is plenty fast.
//
// pls_gcv_cpp:  penalised least squares with a single shared smoothing
//   parameter selected by GCV on a log-spaced grid. The R caller
//   builds the augmented design X (intercept | smooth bases |
//   parametric blocks) and a block-diagonal penalty S so the kernel
//   only solves (X'X + lambda S) beta = X'y for each lambda and reports
//   RSS / edf at the GCV-optimal lambda.
//
// lmm_profile_cpp:  Gaussian random-intercept LMM by profile maximum
//   likelihood over theta = sigma_b^2 / sigma_e^2. The variance
//   structure is block-diagonal (one block per group), so per-theta
//   evaluations cost O(p^2) plus an O(G p) update — independent of n
//   after the initial sufficient-statistic sweep. Theta is chosen on a
//   log-spaced grid plus an explicit theta = 0 (lm) reference.

#include <Rcpp.h>
#include <cmath>
#include <vector>

using namespace Rcpp;

// In-place Cholesky decomposition of a symmetric positive-definite
// matrix A (n x n, column-major). On success, the lower triangle of
// A holds the Cholesky factor L; the upper triangle is zeroed for
// hygiene. Returns 0 on success, -1 if a non-positive pivot is hit.
static int chol_in_place(double* A, int n) {
  for (int j = 0; j < n; ++j) {
    double diag = A[j + j * n];
    for (int k = 0; k < j; ++k) {
      double l_jk = A[j + k * n];
      diag -= l_jk * l_jk;
    }
    if (!(diag > 0.0)) return -1;
    double l_jj = std::sqrt(diag);
    A[j + j * n] = l_jj;
    double inv = 1.0 / l_jj;
    for (int i = j + 1; i < n; ++i) {
      double s = A[i + j * n];
      for (int k = 0; k < j; ++k) s -= A[i + k * n] * A[j + k * n];
      A[i + j * n] = s * inv;
    }
    for (int i = 0; i < j; ++i) A[i + j * n] = 0.0;
  }
  return 0;
}

// Solve (L L') x = b in place given the lower-triangular Cholesky
// factor stored in L (n x n, column-major).
static void chol_solve_in_place(const double* L, int n, double* b) {
  for (int i = 0; i < n; ++i) {
    double s = b[i];
    for (int k = 0; k < i; ++k) s -= L[i + k * n] * b[k];
    b[i] = s / L[i + i * n];
  }
  for (int i = n - 1; i >= 0; --i) {
    double s = b[i];
    for (int k = i + 1; k < n; ++k) s -= L[k + i * n] * b[k];
    b[i] = s / L[i + i * n];
  }
}

// One penalised LS fit at a given lambda. Returns ok flag, beta, RSS,
// and edf (trace of the hat matrix). The caller pays for X'X and
// X'y once and reuses them across the lambda grid.
struct PlsResult {
  std::vector<double> beta;
  double rss;
  double edf;
  bool ok;
};

static PlsResult pls_one(int n, int p,
                          const NumericMatrix& X,
                          const NumericVector& y,
                          const std::vector<double>& XtX,
                          const std::vector<double>& Xty,
                          const NumericMatrix& S,
                          double lambda) {
  PlsResult out;
  out.ok = false;
  out.beta.assign(p, 0.0);

  std::vector<double> A(p * p);
  for (int j = 0; j < p; ++j) {
    for (int i = 0; i < p; ++i) {
      A[i + j * p] = XtX[i + j * p] + lambda * S(i, j);
    }
  }
  if (chol_in_place(A.data(), p) != 0) return out;

  std::vector<double> beta = Xty;
  chol_solve_in_place(A.data(), p, beta.data());
  out.beta = beta;

  double rss = 0.0;
  for (int r = 0; r < n; ++r) {
    double yhat = 0.0;
    for (int j = 0; j < p; ++j) yhat += X(r, j) * beta[j];
    double e = y[r] - yhat;
    rss += e * e;
  }
  out.rss = rss;

  // edf = tr(X (X'X + lambda S)^-1 X') = tr(A^-1 X'X). Solve column j
  // of XtX against A and take element j of the result.
  double edf = 0.0;
  std::vector<double> col(p);
  for (int j = 0; j < p; ++j) {
    for (int i = 0; i < p; ++i) col[i] = XtX[i + j * p];
    chol_solve_in_place(A.data(), p, col.data());
    edf += col[j];
  }
  out.edf = edf;
  out.ok = true;
  return out;
}

// [[Rcpp::export]]
List pls_gcv_cpp(NumericMatrix X, NumericVector y, NumericMatrix S,
                  NumericVector log_lambdas) {
  int n = X.nrow();
  int p = X.ncol();
  if (n != y.size() || S.nrow() != p || S.ncol() != p) {
    return List::create(_["ok"] = false);
  }

  std::vector<double> XtX(p * p, 0.0);
  for (int j = 0; j < p; ++j) {
    for (int i = 0; i <= j; ++i) {
      double s = 0.0;
      for (int r = 0; r < n; ++r) s += X(r, i) * X(r, j);
      XtX[i + j * p] = s;
      XtX[j + i * p] = s;
    }
  }
  std::vector<double> Xty(p, 0.0);
  for (int j = 0; j < p; ++j) {
    double s = 0.0;
    for (int r = 0; r < n; ++r) s += X(r, j) * y[r];
    Xty[j] = s;
  }

  int K = log_lambdas.size();
  double best_gcv = R_PosInf;
  PlsResult best;
  best.ok = false;
  double best_lambda = 0.0;

  for (int k = 0; k < K; ++k) {
    double lambda = std::exp(log_lambdas[k]);
    PlsResult f = pls_one(n, p, X, y, XtX, Xty, S, lambda);
    if (!f.ok) continue;
    double denom = (double) n - f.edf;
    if (!(denom > 0.0)) continue;
    double gcv = (double) n * f.rss / (denom * denom);
    if (gcv < best_gcv) {
      best_gcv = gcv;
      best = f;
      best_lambda = lambda;
    }
  }

  if (!best.ok) return List::create(_["ok"] = false);

  return List::create(
    _["ok"]     = true,
    _["lambda"] = best_lambda,
    _["gcv"]    = best_gcv,
    _["beta"]   = best.beta,
    _["rss"]    = best.rss,
    _["edf"]    = best.edf
  );
}

// Profile-likelihood evaluation at one theta for the random-intercept
// LMM. Returns ok, beta, sigma_e2, sigma_b2, loglik. Closed-form
// reduction: the per-group covariance V_j = sigma_e^2 (I_nj + theta J)
// has Sherman-Morrison inverse (I - phi_j J) / sigma_e^2 with
// phi_j = theta / (1 + n_j theta), so X'WX = X'X - sum_j phi_j s_j s_j'
// where s_j is the column-sum of X within group j. Det term is
// sum_j log(1 + n_j theta).
struct LmmEval {
  std::vector<double> beta;
  double sigma_e2;
  double loglik;
  bool ok;
};

static LmmEval lmm_eval(int n, int p, int G,
                         const std::vector<double>& XtX,
                         const std::vector<double>& Xty,
                         double yty,
                         const std::vector<int>& gsize,
                         const std::vector<double>& gy,
                         const std::vector<double>& gX,
                         double theta) {
  LmmEval out;
  out.ok = false;
  out.beta.assign(p, 0.0);

  std::vector<double> phi(G);
  double sum_log_term = 0.0;
  for (int j = 0; j < G; ++j) {
    double denom = 1.0 + (double) gsize[j] * theta;
    phi[j] = theta / denom;
    sum_log_term += std::log(denom);
  }

  std::vector<double> A(p * p);
  for (int j = 0; j < p; ++j) {
    for (int i = 0; i < p; ++i) A[i + j * p] = XtX[i + j * p];
  }
  for (int g = 0; g < G; ++g) {
    double phig = phi[g];
    if (phig == 0.0) continue;
    for (int b = 0; b < p; ++b) {
      double sb = gX[g + b * G];
      for (int a = 0; a < p; ++a) {
        double sa = gX[g + a * G];
        A[a + b * p] -= phig * sa * sb;
      }
    }
  }

  std::vector<double> rhs(p);
  for (int a = 0; a < p; ++a) rhs[a] = Xty[a];
  for (int g = 0; g < G; ++g) {
    double phig = phi[g];
    if (phig == 0.0) continue;
    double tg = gy[g];
    for (int a = 0; a < p; ++a) rhs[a] -= phig * gX[g + a * G] * tg;
  }

  double yWy = yty;
  for (int g = 0; g < G; ++g) {
    double phig = phi[g];
    if (phig == 0.0) continue;
    double tg = gy[g];
    yWy -= phig * tg * tg;
  }

  if (chol_in_place(A.data(), p) != 0) return out;
  std::vector<double> beta = rhs;
  chol_solve_in_place(A.data(), p, beta.data());
  out.beta = beta;

  double rssW = yWy;
  for (int a = 0; a < p; ++a) rssW -= beta[a] * rhs[a];
  if (!(rssW > 0.0)) rssW = 1e-12;

  double sigma_e2 = rssW / (double) n;
  out.sigma_e2 = sigma_e2;
  out.loglik = -0.5 * (double) n * std::log(2.0 * M_PI)
               - 0.5 * (double) n * std::log(sigma_e2)
               - 0.5 * sum_log_term
               - 0.5 * (double) n;
  out.ok = true;
  return out;
}

// [[Rcpp::export]]
List lmm_profile_cpp(NumericMatrix X, NumericVector y, IntegerVector group,
                      int n_groups, NumericVector log_thetas) {
  int n = X.nrow();
  int p = X.ncol();
  int G = n_groups;
  if (n != y.size() || n != group.size() || G < 2) {
    return List::create(_["ok"] = false);
  }

  std::vector<double> XtX(p * p, 0.0);
  for (int j = 0; j < p; ++j) {
    for (int i = 0; i <= j; ++i) {
      double s = 0.0;
      for (int r = 0; r < n; ++r) s += X(r, i) * X(r, j);
      XtX[i + j * p] = s;
      XtX[j + i * p] = s;
    }
  }
  std::vector<double> Xty(p, 0.0);
  for (int j = 0; j < p; ++j) {
    double s = 0.0;
    for (int r = 0; r < n; ++r) s += X(r, j) * y[r];
    Xty[j] = s;
  }
  double yty = 0.0;
  for (int r = 0; r < n; ++r) yty += y[r] * y[r];

  std::vector<int> gsize(G, 0);
  std::vector<double> gy(G, 0.0);
  std::vector<double> gX(G * p, 0.0);
  for (int r = 0; r < n; ++r) {
    int g = group[r];
    if (g < 0 || g >= G) continue;
    gsize[g] += 1;
    gy[g] += y[r];
    for (int a = 0; a < p; ++a) gX[g + a * G] += X(r, a);
  }

  // Always include theta = 0 (the lm reference).
  LmmEval best = lmm_eval(n, p, G, XtX, Xty, yty, gsize, gy, gX, 0.0);
  if (!best.ok) return List::create(_["ok"] = false);
  double best_theta = 0.0;

  int K = log_thetas.size();
  for (int k = 0; k < K; ++k) {
    double th = std::exp(log_thetas[k]);
    LmmEval f = lmm_eval(n, p, G, XtX, Xty, yty, gsize, gy, gX, th);
    if (!f.ok) continue;
    if (f.loglik > best.loglik) {
      best = f;
      best_theta = th;
    }
  }

  return List::create(
    _["ok"]       = true,
    _["theta"]    = best_theta,
    _["sigma_e2"] = best.sigma_e2,
    _["sigma_b2"] = best_theta * best.sigma_e2,
    _["beta"]     = best.beta,
    _["loglik"]   = best.loglik
  );
}
