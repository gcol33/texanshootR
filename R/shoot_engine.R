# Engine: fit one spec, return a tidy result row.
#
# v1 uses .lm.fit() for speed (already C-level). RcppArmadillo +
# RcppParallel land in a follow-up release without changing this API.

fit_spec <- function(df, outcome, spec) {
  mat <- materialise_spec(df, outcome, spec)
  d <- mat$df
  if (nrow(d) < length(spec$subset) + 5L) return(NULL)
  f <- tryCatch(stats::as.formula(mat$formula), error = function(e) NULL)
  if (is.null(f)) return(NULL)

  # model.matrix() drops rows with NAs in the predictors, which
  # silently desyncs X and y. Build a model frame first so X and y
  # are always aligned.
  mf <- tryCatch(stats::model.frame(f, data = d, na.action = stats::na.omit),
                 error = function(e) NULL)
  if (is.null(mf) || nrow(mf) < length(spec$subset) + 5L) return(NULL)
  X <- tryCatch(stats::model.matrix(f, data = mf), error = function(e) NULL)
  if (is.null(X)) return(NULL)
  y <- mf[[outcome]]
  if (!all(is.finite(y))) {
    keep <- is.finite(y)
    X <- X[keep, , drop = FALSE]
    y <- y[keep]
  }
  if (length(y) < ncol(X) + 2L) return(NULL)

  fit <- tryCatch(stats::.lm.fit(X, y), error = function(e) NULL)
  if (is.null(fit)) return(NULL)

  n <- length(y)
  p <- ncol(X)
  res <- fit$residuals
  rss <- sum(res * res)
  tss <- sum((y - mean(y))^2)
  r2  <- if (tss > 0) max(0, 1 - rss / tss) else 0

  # Overall F-test p-value (excluding the intercept).
  df1 <- max(1L, p - 1L)
  df2 <- n - p
  if (df2 < 1L || tss <= 0) {
    p_value <- NA_real_
  } else {
    f_stat <- ((tss - rss) / df1) / (rss / df2)
    p_value <- stats::pf(f_stat, df1, df2, lower.tail = FALSE)
  }

  list(
    formula        = mat$formula,
    n              = n,
    n_terms        = p,
    r_squared      = r2,
    aic            = n * (log(2 * pi) + log(rss / n) + 1) + 2 * p,
    p_value        = p_value,
    dropped        = setdiff(unlist(strsplit(spec$subset_full %||% paste(spec$subset, collapse = ","), ",")),
                             spec$subset),
    n_dropped      = length(spec$subset_full %||% character()) - length(spec$subset),
    outliers_dropped = if (spec$outlier_seed == "none") 0L
                       else as.integer(round(0.05 * nrow(df))),
    subgroup       = if (spec$subgroup_seed == "none") NA_character_ else spec$subgroup_seed,
    family         = "lm"
  )
}

# Highlighted-spec selection. Implements section6b near-miss bias: prefer
# specs whose p-value falls in [0.045, 0.07], breaking ties to land
# closest to 0.051. Among ties, take the highest R^2.
choose_highlight <- function(results) {
  results <- Filter(Negate(is.null), results)
  if (length(results) == 0L) return(NULL)
  ps <- vapply(results, function(r) r$p_value %||% NA_real_, numeric(1))
  r2 <- vapply(results, function(r) r$r_squared %||% NA_real_, numeric(1))

  in_strip <- !is.na(ps) & ps >= 0.045 & ps <= 0.07
  if (any(in_strip)) {
    cand <- which(in_strip)
    dist <- abs(ps[cand] - 0.051)
    cand <- cand[order(dist, -r2[cand])]
    return(results[[cand[1]]])
  }
  # No near-miss strip: take the smallest p with valid value.
  ord <- order(ps, na.last = TRUE)
  results[[ord[1]]]
}
