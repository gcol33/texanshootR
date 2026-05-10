# Engine: dispatch a spec to its family fitter and return a tidy
# result row. The actual fitters live in R/families.R; this file
# stays as the small dispatch + highlight-selection surface.
#
# v1 supports lm (.lm.fit) and glm (glm.fit). GAM (mgcv) and GLMM
# (lme4) plug into FAMILIES as separate fitters in later phases.

fit_spec <- function(df, outcome, spec) {
  fam_name <- spec$family$fitter %||% "lm"
  fitter   <- tryCatch(family_fitter(fam_name), error = function(e) NULL)
  if (is.null(fitter)) return(NULL)
  fitter(df, outcome, spec)
}

# Highlighted-spec selection. Implements the just-cleared bias from
# Brodeur, Cook & Heyes (2020): prefer specs whose p-value falls in
# [0.040, 0.0499] - distinctly under the 0.05 line but visibly close.
# Break ties to land closest to 0.045. Among ties, take the highest R^2.
choose_highlight <- function(results) {
  results <- Filter(Negate(is.null), results)
  if (length(results) == 0L) return(NULL)
  ps <- vapply(results, function(r) r$p_value %||% NA_real_, numeric(1))
  r2 <- vapply(results, function(r) r$r_squared %||% NA_real_, numeric(1))

  in_strip <- !is.na(ps) & ps >= 0.040 & ps < 0.050
  if (any(in_strip)) {
    cand <- which(in_strip)
    dist <- abs(ps[cand] - 0.045)
    cand <- cand[order(dist, -r2[cand])]
    return(results[[cand[1]]])
  }
  # No just-cleared strip: take the smallest p with valid value.
  ord <- order(ps, na.last = TRUE)
  results[[ord[1]]]
}
