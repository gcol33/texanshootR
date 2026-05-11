# Model-family registry and family-specific fitters.
#
# fit_spec() in shoot_engine.R is a thin dispatcher that reads
# spec$family$fitter and calls the registered fitter from FAMILIES.
# Each fitter takes (df, outcome, spec) and returns the same result
# shape so downstream code (choose_highlight, run records, the UI)
# does not need to know which family produced the row.
#
# Result contract (all fitters):
#   list(formula, n, n_terms, r_squared, aic, p_value,
#        dropped, n_dropped, outliers_dropped, subgroup,
#        family, family_meta = list())
#
# - r_squared is McFadden-style pseudo-R^2 for glm (1 - dev/null_dev).
# - p_value is the F-test against intercept for lm and the LRT against
#   intercept-only for glm. Both null-with-intercept references, so
#   choose_highlight() can compare across families.
# - family is a printable string ("lm", "glm:binomial:logit") so
#   favorite_method() in career.R can tabulate it directly.

# Shared materialisation: returns mf/X/y/formula or NULL if the spec
# cannot be fit (degenerate after transforms / outlier drop / subgroup).
build_model_frame <- function(df, outcome, spec, y_override = NULL) {
  mat <- materialise_spec(df, outcome, spec)
  d <- mat$df
  if (!is.null(y_override)) d[[outcome]] <- y_override
  if (nrow(d) < length(spec$subset) + 5L) return(NULL)
  f <- tryCatch(stats::as.formula(mat$formula), error = function(e) NULL)
  if (is.null(f)) return(NULL)

  # model.matrix() drops rows with NAs in the predictors silently,
  # which would desync X and y. Build a model frame first so X and y
  # stay aligned, then materialise X off the frame.
  mf <- tryCatch(stats::model.frame(f, data = d, na.action = stats::na.omit),
                 error = function(e) NULL)
  if (is.null(mf) || nrow(mf) < length(spec$subset) + 5L) return(NULL)
  X <- tryCatch(stats::model.matrix(f, data = mf), error = function(e) NULL)
  if (is.null(X)) return(NULL)
  y <- mf[[outcome]]
  list(mf = mf, X = X, y = y, formula = mat$formula)
}

# Family-independent metadata block, attached after the numeric fit.
spec_metadata <- function(df, spec, n_used) {
  list(
    dropped          = setdiff(unlist(strsplit(
      spec$subset_full %||% paste(spec$subset, collapse = ","), ",")),
      spec$subset),
    n_dropped        = length(spec$subset_full %||% character()) -
                          length(spec$subset),
    outliers_dropped = if (spec$outlier_seed == "none") 0L
                       else as.integer(round(0.05 * nrow(df))),
    # Carry the outlier seed token (e.g. "leverage_top_5pct", "random_5pct")
    # so the print banner's DATA USED section can label the rule, not just
    # report the count. NA when no outlier perturbation was applied.
    outlier_rule     = if (spec$outlier_seed == "none") NA_character_
                       else spec$outlier_seed,
    subgroup         = if (spec$subgroup_seed == "none") NA_character_
                       else spec$subgroup_seed
  )
}

# ---- lm fitter (the original .lm.fit() path) -----------------------

fit_lm <- function(df, outcome, spec) {
  mb <- build_model_frame(df, outcome, spec)
  if (is.null(mb)) return(NULL)
  X <- mb$X; y <- mb$y
  if (!all(is.finite(y))) {
    keep <- is.finite(y)
    X <- X[keep, , drop = FALSE]; y <- y[keep]
  }
  if (length(y) < ncol(X) + 2L) return(NULL)

  fit <- ols_fit_cpp(X, y)
  if (!isTRUE(fit$ok)) return(NULL)

  c(list(
    formula     = mb$formula,
    n           = fit$n,
    n_terms     = fit$p,
    r_squared   = fit$r_squared,
    aic         = fit$aic,
    p_value     = fit$p_value,
    family      = "lm",
    family_meta = list()
  ), spec_metadata(df, spec, fit$n))
}

# Materialise the (X, y) pair an lm spec would feed to ols_fit_cpp.
# Returns NULL if the spec is degenerate after transforms / outliers /
# subgroup or if the design has too few residual degrees of freedom.
# Used by fit_lm_batch() to assemble batches without paying the
# ols_fit_cpp call per spec.
build_lm_design <- function(df, outcome, spec) {
  mb <- build_model_frame(df, outcome, spec)
  if (is.null(mb)) return(NULL)
  X <- mb$X; y <- mb$y
  if (!all(is.finite(y))) {
    keep <- is.finite(y)
    X <- X[keep, , drop = FALSE]; y <- y[keep]
  }
  if (length(y) < ncol(X) + 2L) return(NULL)
  list(X = X, y = y, formula = mb$formula)
}

# Batch-fit a list of lm specs in one C++ crossing. Specs whose design
# does not materialise are returned as NULL slots so the caller can
# preserve original ordering. Returns a list of result rows in the same
# shape as fit_lm().
fit_lm_batch <- function(df, outcome, specs) {
  if (!length(specs)) return(list())

  designs <- lapply(specs, function(s) build_lm_design(df, outcome, s))
  ok      <- !vapply(designs, is.null, logical(1))
  out     <- vector("list", length(specs))

  if (!any(ok)) return(out)

  Xs <- lapply(designs[ok], `[[`, "X")
  ys <- lapply(designs[ok], `[[`, "y")
  fits <- ols_fit_batch_cpp(Xs, ys)

  ok_idx <- which(ok)
  for (j in seq_along(ok_idx)) {
    i   <- ok_idx[j]
    fit <- fits[[j]]
    if (is.null(fit) || !isTRUE(fit$ok)) next
    out[[i]] <- c(list(
      formula     = designs[[i]]$formula,
      n           = fit$n,
      n_terms     = fit$p,
      r_squared   = fit$r_squared,
      aic         = fit$aic,
      p_value     = fit$p_value,
      family      = "lm",
      family_meta = list()
    ), spec_metadata(df, specs[[i]], fit$n))
  }
  out
}

# ---- glm fitter ----------------------------------------------------
#
# Uses stats::glm.fit() directly to match the .lm.fit() aesthetic and
# avoid formula-parse overhead. Family/link comes from spec$family.
# When spec$family$outcome_coerce is non-NULL, the outcome is coerced
# before fitting -- this is the desperation flail (binomial on
# dichotomised continuous, poisson on rounded continuous). The
# resulting family string makes the coercion visible in run records.

fit_glm <- function(df, outcome, spec) {
  fam_spec <- spec$family
  fam_name <- fam_spec$family %||% "gaussian"
  link     <- fam_spec$link %||% glm_default_link(fam_name)
  coerce   <- fam_spec$outcome_coerce  # NULL or "median_dichotomize" / "round_clip"

  fam_obj <- tryCatch(make_family(fam_name, link), error = function(e) NULL)
  if (is.null(fam_obj)) return(NULL)

  # Coerce y first so model.frame sees the version glm.fit will use.
  y_override <- NULL
  if (!is.null(coerce)) {
    y_raw <- df[[outcome]]
    y_override <- coerce_outcome(y_raw, coerce)
    if (is.null(y_override)) return(NULL)
  }

  mb <- build_model_frame(df, outcome, spec, y_override = y_override)
  if (is.null(mb)) return(NULL)
  X <- mb$X; y <- mb$y

  # Drop non-finite y rows alongside X. glm.fit() will error otherwise.
  keep <- is.finite(y)
  if (!all(keep)) { X <- X[keep, , drop = FALSE]; y <- y[keep] }
  if (length(y) < ncol(X) + 2L) return(NULL)

  # Family-specific y validation. Out-of-domain outcomes (negatives for
  # poisson, non-{0,1} for binomial without weights) make glm.fit error;
  # check before fitting.
  if (!y_in_family_domain(y, fam_name)) return(NULL)

  fit <- tryCatch(
    suppressWarnings(stats::glm.fit(X, y, family = fam_obj,
                                     control = stats::glm.control(maxit = 50L))),
    error = function(e) NULL
  )
  if (is.null(fit) || !isTRUE(fit$converged)) return(NULL)

  n <- length(y); p <- ncol(X)
  dev      <- fit$deviance
  null_dev <- fit$null.deviance
  pseudo_r2 <- if (is.finite(null_dev) && null_dev > 0)
                  max(0, 1 - dev / null_dev) else 0

  # AIC via family$aic when available, otherwise from logLik analogue.
  aic_val <- tryCatch(
    fam_obj$aic(y, n, fit$fitted.values, rep(1, n), dev) + 2 * p,
    error = function(e) NA_real_
  )

  # LRT against intercept-only: chi-square on (null_dev - dev) with
  # df = (df.null - df.residual). Mirrors anova(fit, test="Chisq")'s
  # comparison to the null model.
  df_diff <- fit$df.null - fit$df.residual
  if (df_diff < 1L || !is.finite(null_dev - dev)) {
    p_value <- NA_real_
  } else {
    p_value <- stats::pchisq(null_dev - dev, df = df_diff, lower.tail = FALSE)
  }

  fam_label <- sprintf("glm:%s:%s", fam_name, link)
  if (!is.null(coerce)) fam_label <- paste0(fam_label, ":", coerce)

  c(list(
    formula     = mb$formula,
    n           = n,
    n_terms     = p,
    r_squared   = pseudo_r2,
    aic         = aic_val,
    p_value     = p_value,
    family      = fam_label,
    family_meta = list(family = fam_name, link = link, coerce = coerce,
                        deviance = dev, null_deviance = null_dev)
  ), spec_metadata(df, spec, n))
}

# Resolve a stats::family() object. Returning NULL on unknown link/family
# combinations lets the caller skip the spec gracefully.
make_family <- function(family, link) {
  fn <- switch(family,
    gaussian = stats::gaussian,
    binomial = stats::binomial,
    poisson  = stats::poisson,
    Gamma    = stats::Gamma,
    NULL
  )
  if (is.null(fn)) return(NULL)
  fn(link = link)
}

glm_default_link <- function(family) {
  switch(family,
    gaussian = "identity",
    binomial = "logit",
    poisson  = "log",
    Gamma    = "inverse",
    "identity"
  )
}

y_in_family_domain <- function(y, family) {
  switch(family,
    gaussian = TRUE,
    binomial = all(y >= 0 & y <= 1, na.rm = TRUE),
    poisson  = all(y >= 0 & abs(y - round(y)) < 1e-8, na.rm = TRUE),
    Gamma    = all(y > 0, na.rm = TRUE),
    TRUE
  )
}

# Outcome coercions for the desperation flail. Each returns a numeric
# vector or NULL when the input is not amenable.
coerce_outcome <- function(y, kind) {
  if (!is.numeric(y)) return(NULL)
  if (kind == "median_dichotomize") {
    m <- stats::median(y, na.rm = TRUE)
    if (!is.finite(m)) return(NULL)
    return(as.integer(y > m))
  }
  if (kind == "round_clip") {
    out <- pmax(0L, as.integer(round(y)))
    return(out)
  }
  NULL
}

# ---- gam fitter (native, Gaussian) ---------------------------------
#
# Penalised B-spline regression. For each numeric predictor with enough
# distinct values we generate a cubic B-spline basis and pair it with a
# 2nd-difference penalty (P-spline, Eilers & Marx 1996). Categorical and
# low-cardinality numeric predictors enter parametrically. A single
# shared smoothing parameter is selected by GCV on a log-spaced grid in
# pls_gcv_cpp(); per-smooth lambdas would need multi-D optimisation and
# would be overkill for the satirical fitting volume.
#
# Family is fixed at gaussian-identity. The selector deliberately maps
# every outcome shape onto gaussian for gam picks; the punch is "I used
# s()", not "I picked a clever link". Out-of-family specs (binomial /
# poisson) coerce to gaussian here rather than getting dropped.

fit_gam <- function(df, outcome, spec) {
  mat <- materialise_spec(df, outcome, spec)
  d <- mat$df
  if (nrow(d) < length(spec$subset) + 5L) return(NULL)
  if (!(outcome %in% names(d))) return(NULL)
  y <- d[[outcome]]
  if (!is.numeric(y)) return(NULL)
  keep <- is.finite(y)
  d <- d[keep, , drop = FALSE]
  y <- y[keep]
  if (length(y) < length(spec$subset) + 5L) return(NULL)

  build <- build_gam_design(d, spec, outcome)
  if (is.null(build)) return(NULL)
  X <- build$X
  S <- build$S
  if (ncol(X) < 1L || any(!is.finite(X))) return(NULL)

  log_lambdas <- seq(log(1e-4), log(1e6), length.out = 25L)
  res <- pls_gcv_cpp(X, y, S, log_lambdas)
  if (!isTRUE(res$ok)) return(NULL)

  n   <- length(y)
  rss <- res$rss
  edf <- res$edf
  tss <- sum((y - mean(y))^2)
  r2  <- if (tss > 0) max(0, 1 - rss / tss) else 0

  # F-test against intercept-only with effective DF.
  df1 <- max(1, edf - 1)
  df2 <- n - edf
  if (df2 < 1 || tss <= 0) {
    p_value <- NA_real_
  } else {
    f_stat  <- ((tss - rss) / df1) / (rss / df2)
    p_value <- stats::pf(f_stat, df1, df2, lower.tail = FALSE)
  }

  aic_val <- n * (log(2 * pi) + log(rss / n) + 1) + 2 * edf
  fam_label <- "gam:gaussian:identity"

  c(list(
    formula     = build$formula,
    n           = n,
    n_terms     = max(1L, as.integer(round(edf))),
    r_squared   = r2,
    aic         = aic_val,
    p_value     = p_value,
    family      = fam_label,
    family_meta = list(family = "gaussian", link = "identity",
                        edf = edf, lambda = res$lambda,
                        smoothed = build$smoothed)
  ), spec_metadata(df, spec, n))
}

# Build the augmented design matrix and block-diagonal penalty for a
# gam spec. Numeric predictors with enough distinct values become
# cubic B-spline blocks with 2nd-difference penalty; everything else
# (categoricals, low-card numerics, the intercept) is unpenalised.
# Returns NULL if no usable predictors materialise.
build_gam_design <- function(d, spec, outcome) {
  blocks         <- list(matrix(1, nrow(d), 1,
                                 dimnames = list(NULL, "(Intercept)")))
  penalty_blocks <- list(matrix(0, 1, 1))
  formula_terms  <- character()
  smoothed       <- character()

  for (v in spec$subset) {
    x <- d[[v]]
    if (is.numeric(x)) {
      uv <- unique(x[is.finite(x)])
      if (length(uv) >= 10L) {
        k <- min(8L, length(uv) - 1L)
        B <- tryCatch(bspline_basis(x, k = k),
                       error = function(e) NULL)
        if (is.null(B) || any(!is.finite(B))) next
        colnames(B) <- sprintf("%s.s%d", v, seq_len(ncol(B)))
        blocks[[length(blocks) + 1L]]                 <- B
        penalty_blocks[[length(penalty_blocks) + 1L]] <- second_diff_penalty(ncol(B))
        smoothed      <- c(smoothed, v)
        formula_terms <- c(formula_terms,
                            sprintf("s(%s, k = %d)", v, k))
        next
      }
      M <- matrix(x, ncol = 1L, dimnames = list(NULL, v))
    } else if (is.factor(x) || is.character(x)) {
      f <- as.factor(x)
      if (length(levels(f)) < 2L) next
      d_tmp <- data.frame(.f = f)
      M <- stats::model.matrix(~ .f, data = d_tmp)[, -1L, drop = FALSE]
      colnames(M) <- paste0(v, levels(f)[-1L])
    } else {
      next
    }
    blocks[[length(blocks) + 1L]]                 <- M
    penalty_blocks[[length(penalty_blocks) + 1L]] <- matrix(0, ncol(M), ncol(M))
    formula_terms <- c(formula_terms, v)
  }

  if (length(spec$interactions) && any(nzchar(spec$interactions))) {
    for (it in spec$interactions[nzchar(spec$interactions)]) {
      vars <- strsplit(it, ":")[[1]]
      vars <- vars[vars %in% names(d)]
      if (length(vars) < 2L) next
      cols <- lapply(vars, function(v) {
        x <- d[[v]]
        if (is.numeric(x)) x else as.numeric(as.factor(x))
      })
      x_int <- Reduce(`*`, cols)
      M <- matrix(x_int, ncol = 1L,
                   dimnames = list(NULL, paste(vars, collapse = ":")))
      blocks[[length(blocks) + 1L]]                 <- M
      penalty_blocks[[length(penalty_blocks) + 1L]] <- matrix(0, 1, 1)
      formula_terms <- c(formula_terms, it)
    }
  }

  X <- do.call(cbind, blocks)
  S <- block_diag_dense(penalty_blocks)
  if (length(formula_terms) == 0L) formula_terms <- "1"

  list(X = X, S = S, smoothed = smoothed,
       formula = paste(outcome, "~", paste(formula_terms, collapse = " + ")))
}

# Cubic B-spline basis with k columns, knots placed at evenly-spaced
# quantiles of the data. Wraps splines::bs() (recommended package, so
# always available) with intercept = FALSE so the column space pairs
# cleanly with the explicit intercept term in the design.
bspline_basis <- function(x, k = 8L, degree = 3L) {
  k <- as.integer(k)
  if (k < degree + 1L) k <- as.integer(degree + 1L)
  splines::bs(x, df = k, degree = degree, intercept = FALSE)
}

# 2nd-difference penalty matrix: D2'D2 where D2 is the (k-2 x k)
# 2nd-difference operator. Penalises wiggliness in the spline
# coefficients (Eilers & Marx 1996).
second_diff_penalty <- function(k) {
  if (k <= 2L) return(matrix(0, k, k))
  D <- diff(diag(k), differences = 2L)
  crossprod(D)
}

# Manual block-diagonal builder. Avoids a Matrix dependency. Mats may
# be 1x1; zero-row blocks are skipped.
block_diag_dense <- function(mats) {
  ns <- vapply(mats, function(m) nrow(m), integer(1))
  total <- sum(ns)
  out <- matrix(0, total, total)
  off <- 0L
  for (m in mats) {
    nr <- nrow(m); nc <- ncol(m)
    if (nr == 0L || nc == 0L) next
    out[(off + 1L):(off + nr), (off + 1L):(off + nc)] <- m
    off <- off + nr
  }
  out
}

# ---- glmm fitter (native, Gaussian random intercept) ---------------
#
# Random-intercept LMM with Gaussian outcome. The variance ratio
# theta = sigma_b^2 / sigma_e^2 is selected by profile maximum
# likelihood on a log-spaced grid in lmm_profile_cpp(); theta = 0
# (the lm reference) is always evaluated so the LRT against the
# fixed-only null is consistent.
#
# Family is fixed at gaussian-identity. As with gam, the joke is "I
# used a mixed model"; the fitter coerces non-Gaussian specs to
# gaussian rather than rejecting them.

fit_glmm <- function(df, outcome, spec) {
  group <- find_glmm_group(df, outcome, spec)
  if (is.null(group)) return(NULL)

  mat <- materialise_spec(df, outcome, spec)
  d <- mat$df
  if (!(group %in% names(d))) return(NULL)
  if (!(outcome %in% names(d))) return(NULL)
  if (nrow(d) < length(spec$subset) + 10L) return(NULL)

  d[[group]] <- as.factor(d[[group]])
  G <- length(levels(d[[group]]))
  if (G < 3L) return(NULL)

  y <- d[[outcome]]
  if (!is.numeric(y)) return(NULL)
  ok <- is.finite(y)
  d <- d[ok, , drop = FALSE]
  y <- y[ok]
  if (length(y) < length(spec$subset) + 10L) return(NULL)

  build <- build_glmm_design(d, spec, outcome, group)
  if (is.null(build)) return(NULL)
  X      <- build$X
  g_int  <- build$group_int
  G_used <- build$n_groups
  if (G_used < 3L) return(NULL)
  if (any(!is.finite(X))) return(NULL)

  log_thetas <- seq(log(1e-3), log(1e3), length.out = 25L)
  res <- lmm_profile_cpp(X, y, g_int, G_used, log_thetas)
  if (!isTRUE(res$ok)) return(NULL)

  # Null fit (theta = 0 -> ordinary lm). Compute residual variance and
  # Gaussian loglik analytically rather than calling lm() so the LRT
  # uses identical sufficient statistics.
  null_res <- lmm_profile_cpp(X, y, g_int, G_used, numeric(0))
  if (!isTRUE(null_res$ok)) return(NULL)

  n        <- length(y)
  ll       <- res$loglik
  null_ll  <- null_res$loglik
  delta_ll <- if (is.finite(ll) && is.finite(null_ll)) ll - null_ll
              else NA_real_

  # 50:50 mixture of chi^2_0 and chi^2_1: standard p-value correction
  # for testing a single variance component on the boundary theta = 0.
  p_value <- if (is.finite(delta_ll) && delta_ll > 0)
                0.5 * stats::pchisq(2 * delta_ll, df = 1L,
                                     lower.tail = FALSE)
             else NA_real_

  pseudo_r2 <- if (is.finite(delta_ll) && delta_ll > 0 && n > 0)
                  max(0, 1 - exp(-2 * delta_ll / n))
               else 0

  # AIC = -2 ll + 2 * (p_fixed + 2 variance components).
  aic_val <- -2 * ll + 2 * (ncol(X) + 2L)

  fixed_rhs <- if (length(spec$subset) == 0L) "1"
               else paste(spec$subset, collapse = " + ")
  if (length(spec$interactions) && any(nzchar(spec$interactions))) {
    its <- spec$interactions[nzchar(spec$interactions)]
    fixed_rhs <- paste(fixed_rhs, paste(its, collapse = " + "),
                        sep = " + ")
  }
  formula_str <- sprintf("%s ~ %s + (1 | %s)", outcome, fixed_rhs, group)

  c(list(
    formula     = formula_str,
    n           = n,
    n_terms     = ncol(X) + 1L,
    r_squared   = pseudo_r2,
    aic         = aic_val,
    p_value     = p_value,
    family      = "glmm:gaussian:identity",
    family_meta = list(family = "gaussian", link = "identity",
                        group = group, theta = res$theta,
                        sigma_e2 = res$sigma_e2, sigma_b2 = res$sigma_b2,
                        loglik = ll, null_loglik = null_ll)
  ), spec_metadata(df, spec, n))
}

# Build the fixed-effect design matrix for a glmm spec plus the
# 0-indexed group integer vector that the kernel expects. Categorical
# fixed-effect predictors are dummy-coded; numeric ones enter linearly.
build_glmm_design <- function(d, spec, outcome, group) {
  blocks <- list(matrix(1, nrow(d), 1,
                         dimnames = list(NULL, "(Intercept)")))

  for (v in spec$subset) {
    if (identical(v, group)) next
    x <- d[[v]]
    if (is.numeric(x)) {
      M <- matrix(x, ncol = 1L, dimnames = list(NULL, v))
    } else if (is.factor(x) || is.character(x)) {
      f <- as.factor(x)
      if (length(levels(f)) < 2L) next
      d_tmp <- data.frame(.f = f)
      M <- stats::model.matrix(~ .f, data = d_tmp)[, -1L, drop = FALSE]
      colnames(M) <- paste0(v, levels(f)[-1L])
    } else {
      next
    }
    blocks[[length(blocks) + 1L]] <- M
  }

  if (length(spec$interactions) && any(nzchar(spec$interactions))) {
    for (it in spec$interactions[nzchar(spec$interactions)]) {
      vars <- strsplit(it, ":")[[1]]
      vars <- vars[vars %in% names(d) & vars != group]
      if (length(vars) < 2L) next
      cols <- lapply(vars, function(v) {
        x <- d[[v]]
        if (is.numeric(x)) x else as.numeric(as.factor(x))
      })
      x_int <- Reduce(`*`, cols)
      M <- matrix(x_int, ncol = 1L,
                   dimnames = list(NULL, paste(vars, collapse = ":")))
      blocks[[length(blocks) + 1L]] <- M
    }
  }

  X <- do.call(cbind, blocks)
  group_int <- as.integer(d[[group]]) - 1L
  list(X = X, group_int = group_int,
       n_groups = length(levels(d[[group]])))
}

# ---- sem fitter (mediation, native, no optimisation) ---------------
#
# Single-mediator path model X -> M -> Y. Fitted as two OLS regressions
# via stats::.lm.fit() (same primitive as fit_lm) and a Sobel test for
# the indirect effect a*b. Spec convention:
#   spec$subset[1] = X     (independent variable)
#   spec$subset[2] = M     (mediator)
#   spec$subset[-(1:2)]    = optional Y-equation covariates that also
#                            enter the M equation as controls
# A spec with fewer than two numeric predictors returns NULL — the
# mediation move requires at least one X and one M.
#
# The reported p-value is the two-sided Sobel z for ab. r_squared and
# AIC come from the Y equation so the row stays comparable with the
# other families' single-equation summaries. Sobel's independence
# assumption between a-hat and b-hat is the standard approximation.

fit_sem <- function(df, outcome, spec) {
  if (length(spec$subset) < 2L) return(NULL)
  X_var <- spec$subset[[1L]]
  M_var <- spec$subset[[2L]]
  controls <- spec$subset[-c(1L, 2L)]
  if (X_var == M_var) return(NULL)

  mat <- materialise_spec(df, outcome, spec)
  d <- mat$df
  if (!(outcome %in% names(d)) ||
      !(X_var %in% names(d)) ||
      !(M_var %in% names(d))) return(NULL)

  y <- d[[outcome]]; xv <- d[[X_var]]; mv <- d[[M_var]]
  if (!is.numeric(y) || !is.numeric(xv) || !is.numeric(mv)) return(NULL)

  ctrl_cols <- list()
  for (cv in controls) {
    if (!(cv %in% names(d))) next
    z <- d[[cv]]
    if (is.numeric(z)) {
      ctrl_cols[[cv]] <- z
    } else if (is.factor(z) || is.character(z)) {
      f <- as.factor(z)
      if (length(levels(f)) < 2L) next
      M <- stats::model.matrix(~ f)[, -1L, drop = FALSE]
      colnames(M) <- paste0(cv, levels(f)[-1L])
      for (cc in colnames(M)) ctrl_cols[[cc]] <- M[, cc]
    }
  }

  ok <- is.finite(y) & is.finite(xv) & is.finite(mv)
  if (length(ctrl_cols)) {
    for (cc in names(ctrl_cols)) ok <- ok & is.finite(ctrl_cols[[cc]])
  }
  if (sum(ok) < 3L + length(ctrl_cols) + 5L) return(NULL)
  y <- y[ok]; xv <- xv[ok]; mv <- mv[ok]
  if (length(ctrl_cols))
    ctrl_cols <- lapply(ctrl_cols, function(z) z[ok])

  intercept <- rep(1, length(y))
  ctrl_mat <- if (length(ctrl_cols))
                do.call(cbind, ctrl_cols) else NULL

  # Equation a: M ~ X (+ controls)
  Xa <- if (is.null(ctrl_mat)) cbind(intercept, X = xv)
        else                    cbind(intercept, X = xv, ctrl_mat)
  fit_a <- tryCatch(stats::.lm.fit(Xa, mv), error = function(e) NULL)
  if (is.null(fit_a)) return(NULL)
  a_hat  <- fit_a$coefficients[2L]
  res_a  <- fit_a$residuals
  rss_a  <- sum(res_a * res_a)
  df_a   <- length(mv) - ncol(Xa)
  if (df_a < 1L) return(NULL)
  sigma2_a <- rss_a / df_a
  XtX_inv_a <- tryCatch(chol2inv(chol(crossprod(Xa))),
                          error = function(e) NULL)
  if (is.null(XtX_inv_a)) return(NULL)
  var_a <- sigma2_a * XtX_inv_a[2L, 2L]
  if (!is.finite(var_a) || var_a <= 0) return(NULL)

  # Equation b/c': Y ~ X + M (+ controls)
  Xb <- if (is.null(ctrl_mat)) cbind(intercept, X = xv, M = mv)
        else                    cbind(intercept, X = xv, M = mv, ctrl_mat)
  fit_b <- tryCatch(stats::.lm.fit(Xb, y), error = function(e) NULL)
  if (is.null(fit_b)) return(NULL)
  c_prime <- fit_b$coefficients[2L]
  b_hat   <- fit_b$coefficients[3L]
  res_b   <- fit_b$residuals
  rss_b   <- sum(res_b * res_b)
  n       <- length(y)
  df_b    <- n - ncol(Xb)
  if (df_b < 1L) return(NULL)
  sigma2_b <- rss_b / df_b
  XtX_inv_b <- tryCatch(chol2inv(chol(crossprod(Xb))),
                          error = function(e) NULL)
  if (is.null(XtX_inv_b)) return(NULL)
  var_b <- sigma2_b * XtX_inv_b[3L, 3L]
  if (!is.finite(var_b) || var_b <= 0) return(NULL)

  ab <- a_hat * b_hat
  sobel_se <- sqrt(b_hat * b_hat * var_a + a_hat * a_hat * var_b)
  if (!is.finite(sobel_se) || sobel_se <= 0) {
    p_value <- NA_real_
    sobel_z <- NA_real_
  } else {
    sobel_z <- ab / sobel_se
    p_value <- 2 * stats::pnorm(-abs(sobel_z))
  }

  tss_y <- sum((y - mean(y))^2)
  r2 <- if (tss_y > 0) max(0, 1 - rss_b / tss_y) else 0
  aic_val <- n * (log(2 * pi) + log(rss_b / n) + 1) + 2 * ncol(Xb)

  control_label <- if (length(controls)) paste(" +", paste(controls, collapse = " + "))
                   else ""
  formula_str <- sprintf("%s <- %s (via %s)%s",
                          outcome, X_var, M_var, control_label)

  c(list(
    formula     = formula_str,
    n           = n,
    n_terms     = ncol(Xb),
    r_squared   = r2,
    aic         = aic_val,
    p_value     = p_value,
    family      = "sem:mediation",
    family_meta = list(family = "sem", model = "mediation",
                        X = X_var, M = M_var, controls = controls,
                        a = unname(a_hat), b = unname(b_hat),
                        c_prime = unname(c_prime),
                        ab = unname(ab),
                        sobel_z = unname(sobel_z),
                        sobel_se = unname(sobel_se))
  ), spec_metadata(df, spec, n))
}

# ---- cor fitter (bivariate Pearson / Spearman / Kendall) -----------
#
# The bare-correlation move: spec must contain exactly one predictor;
# we report Pearson / Spearman / Kendall r between outcome and that
# predictor, plus the standard test's p-value. The method is carried on
# spec$family$method so the selector controls which flavour gets used.

fit_cor <- function(df, outcome, spec) {
  if (length(spec$subset) != 1L) return(NULL)
  pred <- spec$subset[[1L]]
  method <- spec$family$method %||% "pearson"
  if (!(method %in% c("pearson", "spearman", "kendall"))) return(NULL)

  mat <- materialise_spec(df, outcome, spec)
  d <- mat$df
  if (!(outcome %in% names(d)) || !(pred %in% names(d))) return(NULL)
  y <- d[[outcome]]; x <- d[[pred]]
  if (!is.numeric(y) || !is.numeric(x)) return(NULL)
  ok <- is.finite(y) & is.finite(x)
  if (sum(ok) < 5L) return(NULL)
  y <- y[ok]; x <- x[ok]
  if (stats::sd(y) == 0 || stats::sd(x) == 0) return(NULL)

  ct <- tryCatch(
    suppressWarnings(stats::cor.test(x, y, method = method)),
    error = function(e) NULL
  )
  if (is.null(ct)) return(NULL)
  r <- unname(ct$estimate)
  p <- ct$p.value
  n <- length(y)

  r2 <- if (is.finite(r)) r * r else 0
  # Gaussian-style AIC of a one-parameter "predictor of y" model. Lets
  # the row sit on the same scale as the lm AICs in choose_highlight.
  rss <- (1 - r2) * sum((y - mean(y))^2)
  aic_val <- if (rss > 0)
                n * (log(2 * pi) + log(rss / n) + 1) + 2 * 2L
             else NA_real_

  fam_label <- sprintf("cor:%s", method)

  c(list(
    formula     = sprintf("%s ~ %s", outcome, pred),
    n           = n,
    n_terms     = 2L,
    r_squared   = r2,
    aic         = aic_val,
    p_value     = p,
    family      = fam_label,
    family_meta = list(family = "cor", method = method, r = r)
  ), spec_metadata(df, spec, n))
}

# ---- wls fitter (heteroscedasticity-corrected lm, two-stage) -------
#
# Standard two-stage feasible WLS: fit OLS, regress |residuals| on the
# same predictors to get a sigma trend, refit on sqrt(W)*X and
# sqrt(W)*y with W = 1 / sigma_hat^2. The result row reports the
# weighted F-test against the intercept-only model and a weighted R^2.
# All linear algebra goes through stats::.lm.fit() — the same primitive
# fit_lm uses — so no new C++ surface is needed.

fit_wls <- function(df, outcome, spec) {
  mb <- build_model_frame(df, outcome, spec)
  if (is.null(mb)) return(NULL)
  X <- mb$X; y <- mb$y
  if (!all(is.finite(y))) {
    keep <- is.finite(y)
    X <- X[keep, , drop = FALSE]; y <- y[keep]
  }
  if (length(y) < ncol(X) + 5L) return(NULL)

  # Stage 1: ordinary lm.
  fit1 <- tryCatch(stats::.lm.fit(X, y), error = function(e) NULL)
  if (is.null(fit1)) return(NULL)
  abs_resid <- abs(fit1$residuals)
  if (any(!is.finite(abs_resid))) return(NULL)
  if (max(abs_resid) <= .Machine$double.eps * 4) return(NULL)

  # Stage 2: regress |resid| on X to estimate the sigma trend. Use the
  # fitted values as sigma_hat, clipped from below so weights stay
  # finite even when the trend dips to zero on the boundary. .lm.fit()
  # returns residuals but not fitted.values, so we reconstruct
  # fitted = y - residuals.
  fit2 <- tryCatch(stats::.lm.fit(X, abs_resid), error = function(e) NULL)
  if (is.null(fit2)) return(NULL)
  sigma_hat <- abs_resid - fit2$residuals
  floor_s <- max(1e-6 * stats::median(abs_resid, na.rm = TRUE),
                  .Machine$double.eps)
  sigma_hat <- pmax(sigma_hat, floor_s)
  w <- 1 / (sigma_hat * sigma_hat)
  sw <- sqrt(w)

  # Stage 3: weighted lm via sqrt(w) rescaling — algebraically identical
  # to solving (X' W X) beta = X' W y, but routed through .lm.fit() for
  # the QR path.
  Xw <- X * sw
  yw <- y * sw
  fit3 <- tryCatch(stats::.lm.fit(Xw, yw), error = function(e) NULL)
  if (is.null(fit3)) return(NULL)

  n <- length(y); p <- ncol(X)
  res_w <- fit3$residuals
  rss_w <- sum(res_w * res_w)
  # Weighted total SS (against the weighted-mean reference).
  y_w_mean <- sum(w * y) / sum(w)
  tss_w <- sum(w * (y - y_w_mean)^2)
  r2 <- if (tss_w > 0) max(0, 1 - rss_w / tss_w) else 0

  df1 <- max(1L, p - 1L)
  df2 <- n - p
  if (df2 < 1L || tss_w <= 0) {
    p_value <- NA_real_
  } else {
    f_stat <- ((tss_w - rss_w) / df1) / (rss_w / df2)
    p_value <- stats::pf(f_stat, df1, df2, lower.tail = FALSE)
  }

  # AIC of the weighted Gaussian model: n*log(2*pi) + n*log(rss_w/n) +
  # n + 2*p - sum(log(w)). The log-weight correction makes the AIC
  # comparable in scale to fit_lm.
  aic_val <- n * (log(2 * pi) + log(rss_w / n) + 1) + 2 * p -
              sum(log(w))

  c(list(
    formula     = mb$formula,
    n           = n,
    n_terms     = p,
    r_squared   = r2,
    aic         = aic_val,
    p_value     = p_value,
    family      = "wls",
    family_meta = list(family = "wls",
                        stage2_r2 =
                          if (sum(abs_resid^2) > 0)
                            max(0, 1 - sum(fit2$residuals^2) /
                                       sum((abs_resid - mean(abs_resid))^2))
                          else 0,
                        weight_floor = floor_s)
  ), spec_metadata(df, spec, n))
}

# Pick a column suitable as a random-intercept grouping factor: outside
# the spec's fixed-effect subset, with a small-but-not-trivial number of
# distinct levels. Returns the column name or NULL.
find_glmm_group <- function(df, outcome, spec) {
  cand <- setdiff(names(df), c(outcome, spec$subset))
  if (length(cand) == 0L) return(NULL)
  n <- nrow(df)
  cap <- max(3L, n %/% 3L)
  for (v in cand) {
    x <- df[[v]]
    levs <- unique(x[!is.na(x)])
    n_levs <- length(levs)
    if (n_levs < 3L || n_levs > cap) next
    if (is.factor(x) || is.character(x)) return(v)
    if (is.numeric(x) && all(abs(x - round(x)) < 1e-8, na.rm = TRUE))
      return(v)
  }
  NULL
}

# ---- registry ------------------------------------------------------

# Shared batch scaffold. Every non-lm family currently routes through
# this loop -- one R-level iteration per spec, calling the family's
# single-spec fitter. The named fit_*_batch wrappers below are the
# insertion points for future C++ batch kernels: replacing a wrapper
# body with a single call to a `*_fit_batch_cpp` kernel collapses the
# group into one R/C++ crossing without touching the dispatcher in
# fit_specs_batch() or the FAMILIES registry.
batch_via_loop <- function(df, outcome, specs, single_fitter) {
  lapply(specs, function(s) single_fitter(df, outcome, s))
}

# TODO(C++ batch): replace body with a glm_fit_batch_cpp() kernel that
# runs IRLS per spec, dispatches family/link internally, and returns the
# deviance / null-deviance / LRT pieces that fit_glm() reads.
fit_glm_batch <- function(df, outcome, specs) {
  batch_via_loop(df, outcome, specs, fit_glm)
}

# TODO(C++ batch): replace body with a wls_fit_batch_cpp() kernel that
# does the two-stage (OLS -> |resid| trend -> weighted refit) in one
# crossing, mirroring fit_wls().
fit_wls_batch <- function(df, outcome, specs) {
  batch_via_loop(df, outcome, specs, fit_wls)
}

# TODO(C++ batch): pls_gcv_cpp() already does the per-spec heavy lift;
# a gam_fit_batch_cpp() that builds the B-spline + penalty stack inside
# C++ would amortise the basis-construction + R/C++ crossing across the
# group.
fit_gam_batch <- function(df, outcome, specs) {
  batch_via_loop(df, outcome, specs, fit_gam)
}

# TODO(C++ batch): lmm_profile_cpp() is the single-spec kernel; a
# glmm_fit_batch_cpp() that loops the profile likelihood in C++ keeps
# the optimisation state hot across specs.
fit_glmm_batch <- function(df, outcome, specs) {
  batch_via_loop(df, outcome, specs, fit_glmm)
}

# TODO(C++ batch): bivariate Pearson/Spearman/Kendall is small and
# pure -- a cor_fit_batch_cpp() that loops three correlation routines
# is the cheapest kernel to write and the cleanest proof of the
# scaffold.
fit_cor_batch <- function(df, outcome, specs) {
  batch_via_loop(df, outcome, specs, fit_cor)
}

# TODO(C++ batch): linear SEM (path / mediation) reduces to a sequence
# of OLS fits; a sem_fit_batch_cpp() could reuse ols_fit_batch_cpp()
# internally for the inner regressions.
fit_sem_batch <- function(df, outcome, specs) {
  batch_via_loop(df, outcome, specs, fit_sem)
}

FAMILIES <- list(
  lm   = list(fitter = fit_lm,   batch_fitter = fit_lm_batch,
              available = function() TRUE,
              min_career = "Junior Researcher"),
  cor  = list(fitter = fit_cor,  batch_fitter = fit_cor_batch,
              available = function() TRUE,
              min_career = "Postdoc"),
  glm  = list(fitter = fit_glm,  batch_fitter = fit_glm_batch,
              available = function() TRUE,
              min_career = "Postdoc"),
  wls  = list(fitter = fit_wls,  batch_fitter = fit_wls_batch,
              available = function() TRUE,
              min_career = "Senior Scientist"),
  gam  = list(fitter = fit_gam,  batch_fitter = fit_gam_batch,
              available = function() TRUE,
              min_career = "Senior Scientist"),
  glmm = list(fitter = fit_glmm, batch_fitter = fit_glmm_batch,
              available = function() TRUE,
              min_career = "PI"),
  sem  = list(fitter = fit_sem,  batch_fitter = fit_sem_batch,
              available = function() TRUE,
              min_career = "PI")
)

family_fitter <- function(name) {
  reg <- FAMILIES[[name]]
  if (is.null(reg)) stop(sprintf("Unknown model family: %s", name),
                          call. = FALSE)
  reg$fitter
}

family_batch_fitter <- function(name) {
  reg <- FAMILIES[[name]]
  if (is.null(reg)) stop(sprintf("Unknown model family: %s", name),
                          call. = FALSE)
  reg$batch_fitter
}

available_families <- function(career_level) {
  out <- character()
  for (name in names(FAMILIES)) {
    reg <- FAMILIES[[name]]
    if (!isTRUE(reg$available())) next
    if (tier_index(career_level) < tier_index(reg$min_career)) next
    out <- c(out, name)
  }
  if (length(out) == 0L) "lm" else out
}
