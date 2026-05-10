# Pure family selector. Decides which model family to attach to a
# spec before it is fit, based on:
#   - career_level     (gates which fitters are available at all)
#   - escalating       (desperation phase => glm bias + occasional flail)
#   - outcome vector   (compatibility checks: binary -> binomial, etc.)
#   - available pool   (output of available_families(career_level))
#   - RNG              (called inside set.seed-controlled paths)
#
# Returns a list ready to assign to spec$family, of one of these shapes:
#   list(fitter = "lm")
#   list(fitter = "glm", family = "binomial", link = "logit")
#   list(fitter = "glm", family = "binomial", link = "logit",
#        outcome_coerce = "median_dichotomize")
#
# The selector is the joke: at low career it sticks to lm. As tiers
# rise it leans on glm more often. Under desperation it preferentially
# reaches for glm, and a small fraction of the time it picks a family
# that doesn't match the outcome and forces a coercion -- the shooter
# trying logistic regression on a continuous outcome to chase the
# next-decimal p-value.

# Per-tier probability of upgrading from lm to glm during normal search.
# Junior is locked to lm regardless. PI defaults to glm only ~half the
# time -- the search still wants lm in the pool because lm's F-test
# strip is where the just-cleared bias does its work.
GLM_PROB_BY_TIER <- c(
  "Junior Researcher" = 0.00,
  "Postdoc"           = 0.15,
  "Senior Scientist"  = 0.35,
  "PI"                = 0.55
)

# Bias when escalating: any tier that has an upgrade family available
# leans on it heavily. The mismatched-flail roll happens within this
# branch and only applies to glm picks (gam/glmm don't carry the
# coercion knob).
ESCALATION_GLM_PROB    <- 0.85
ESCALATION_FLAIL_PROB  <- 0.20

# Within the "upgrade beyond lm" branch, weights for each upgrade
# family. glm dominates because it's the most often-applicable; gam,
# glmm, and sem get meaningful air time at the tiers where they're
# unlocked. cor (bare-correlation give-up move) and wls
# (heteroscedasticity flex) sit alongside glm in availability but with
# smaller weight so glm stays the default upgrade.
UPGRADE_WEIGHTS <- c(glm = 1.0, cor = 0.3, wls = 0.5,
                      gam = 0.6, glmm = 0.4, sem = 0.3)

select_family <- function(career_level,
                          escalating = FALSE,
                          outcome    = NULL,
                          available  = NULL) {
  if (is.null(available)) available <- available_families(career_level)
  upgrades <- intersect(names(UPGRADE_WEIGHTS), available)
  if (length(upgrades) == 0L) return(list(fitter = "lm"))

  prob <- if (isTRUE(escalating)) ESCALATION_GLM_PROB
          else GLM_PROB_BY_TIER[[career_level]] %||% 0
  if (stats::runif(1) >= prob) return(list(fitter = "lm"))

  pick <- if (length(upgrades) == 1L) upgrades
          else sample(upgrades, size = 1L,
                      prob = UPGRADE_WEIGHTS[upgrades])

  outcome_kind <- classify_outcome(outcome)

  if (pick == "glm") {
    flail <- isTRUE(escalating) && stats::runif(1) < ESCALATION_FLAIL_PROB
    return(pick_glm_family(outcome_kind, flail = flail))
  }
  if (pick == "gam")  return(pick_gam_family(outcome_kind))
  if (pick == "glmm") return(pick_glmm_family(outcome_kind))
  if (pick == "sem")  return(pick_sem_family())
  if (pick == "cor")  return(pick_cor_family())
  if (pick == "wls")  return(pick_wls_family())

  list(fitter = "lm")
}

# Categorise the outcome vector for family compatibility:
#   "binary"     - all values in {0, 1}
#   "count"      - non-negative integers
#   "positive"   - strictly positive numeric (Gamma candidate)
#   "continuous" - everything else numeric
#   "unknown"    - non-numeric / NULL
classify_outcome <- function(y) {
  if (is.null(y) || !is.numeric(y)) return("unknown")
  y <- y[is.finite(y)]
  if (length(y) == 0L) return("unknown")
  if (all(y %in% c(0, 1))) return("binary")
  if (all(y >= 0) && all(abs(y - round(y)) < 1e-8)) return("count")
  if (all(y > 0)) return("positive")
  "continuous"
}

# Choose a glm family/link based on outcome class. When flail = TRUE,
# pick a family that does NOT match the outcome and force a coercion
# so glm.fit() will accept it -- the desperation tell.
pick_glm_family <- function(outcome_kind, flail = FALSE) {
  if (isTRUE(flail) && outcome_kind %in% c("continuous", "positive")) {
    # The two flails: dichotomise for binomial, round-and-clip for
    # poisson. Either reads as the shooter contorting the data.
    coerce <- sample(c("median_dichotomize", "round_clip"), 1L)
    if (coerce == "median_dichotomize") {
      return(list(fitter = "glm", family = "binomial", link = "logit",
                  outcome_coerce = "median_dichotomize"))
    }
    return(list(fitter = "glm", family = "poisson", link = "log",
                outcome_coerce = "round_clip"))
  }

  switch(outcome_kind,
    binary     = list(fitter = "glm", family = "binomial",
                      link   = sample(c("logit", "probit"), 1L)),
    count      = list(fitter = "glm", family = "poisson", link = "log"),
    positive   = if (stats::runif(1) < 0.5)
                    list(fitter = "glm", family = "Gamma",
                         link   = sample(c("inverse", "log"), 1L))
                 else
                    list(fitter = "glm", family = "gaussian",
                         link   = sample(c("identity", "log"), 1L)),
    continuous = list(fitter = "glm", family = "gaussian",
                      link   = sample(c("identity", "log"), 1L)),
    # outcome unknown: fall back to gaussian-identity, which is just
    # lm in disguise but keeps the family label visible.
    list(fitter = "glm", family = "gaussian", link = "identity")
  )
}

# GAM and GLMM family pickers. Both are deterministic by outcome kind:
# the smooth-vs-parametric choice (gam) and the random-effect choice
# (glmm) carry the "fancier model" punch, so the family/link stays
# canonical instead of doubling up on the desperation knob.
pick_gam_family <- function(outcome_kind) {
  base <- switch(outcome_kind,
    binary     = list(family = "binomial", link = "logit"),
    count      = list(family = "poisson",  link = "log"),
    positive   = list(family = "Gamma",    link = "log"),
    continuous = list(family = "gaussian", link = "identity"),
                 list(family = "gaussian", link = "identity")
  )
  c(list(fitter = "gam"), base)
}

# sem (single-mediator path model): outcome shape doesn't change the
# model class, so the picker is parameter-free. The fitter handles the
# X / M assignment from spec$subset and returns NULL when the spec has
# fewer than two predictors.
pick_sem_family <- function() {
  list(fitter = "sem", model = "mediation")
}

# cor (bare bivariate correlation): roll one of Pearson / Spearman /
# Kendall. The fitter requires a single-predictor spec and returns
# NULL otherwise, so a multi-predictor draw just falls through.
pick_cor_family <- function() {
  list(fitter = "cor",
       method  = sample(c("pearson", "spearman", "kendall"), 1L))
}

# wls (heteroscedasticity-corrected lm): no parameters needed — the
# fitter estimates the variance trend from the OLS residuals.
pick_wls_family <- function() {
  list(fitter = "wls")
}

pick_glmm_family <- function(outcome_kind) {
  base <- switch(outcome_kind,
    binary     = list(family = "binomial", link = "logit"),
    count      = list(family = "poisson",  link = "log"),
    # GLMM Gamma is finicky in lme4; gaussian-log is the conservative
    # "positive outcome" pick.
    positive   = list(family = "gaussian", link = "log"),
    continuous = list(family = "gaussian", link = "identity"),
                 list(family = "gaussian", link = "identity")
  )
  c(list(fitter = "glmm"), base)
}
