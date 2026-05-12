# Phase 2: model_form perturbation axis. Covers:
#   - sample_model_form() produces records with trigger-aware kinds
#   - apply_model_form() projects fitter-switching kinds onto spec$family
#   - materialise_spec() wraps polynomial / spline predictors in the RHS
#   - fit_rlm() returns a well-formed result row
#   - describe_spec() / methods_summary() render each sub-kind

make_phase2_df <- function() {
  set.seed(11)
  n <- 150
  data.frame(
    y     = rnorm(n, 0, 1),
    x1    = rnorm(n, 0, 1),
    x2    = rnorm(n, 0, 1),
    pos   = abs(rnorm(n, 5, 1)) + 0.1,
    grp   = factor(sample(c("A","B","C"), n, TRUE)),
    other = factor(sample(c("p","q","r"), n, TRUE))
  )
}

test_that("sample_model_form returns NULL on a df with no viable column", {
  d <- data.frame(y = c(1, 2, 3), z = c("a", "a", "a"),
                  stringsAsFactors = FALSE)
  spec <- texanshootR:::new_spec("z")
  expect_null(texanshootR:::sample_model_form(d, "y", spec))
})

test_that("sample_model_form yields one of the supported kinds", {
  d <- make_phase2_df()
  spec <- texanshootR:::new_spec(c("x1", "x2"))
  picks <- replicate(40,
                     texanshootR:::sample_model_form(d, "y", spec),
                     simplify = FALSE)
  kinds <- vapply(picks, function(p) if (is.null(p)) NA_character_ else p$kind,
                  character(1))
  expect_true(all(kinds %in% c("polynomial", "spline", "robust",
                                "family_swap", "random_effect")))
  expect_true(length(unique(stats::na.omit(kinds))) >= 2L)
})

test_that("apply_model_form rewrites spec$family for fitter-switching kinds", {
  spec <- texanshootR:::new_spec(c("x1"))
  spec$family <- list(fitter = "lm")

  s_rob <- texanshootR:::apply_model_form(
    spec, list(kind = "robust"))
  expect_equal(s_rob$family$fitter, "rlm")

  s_swap <- texanshootR:::apply_model_form(
    spec, list(kind = "family_swap", family = "poisson", link = "log",
               outcome_coerce = "round_clip"))
  expect_equal(s_swap$family$fitter, "glm")
  expect_equal(s_swap$family$family, "poisson")
  expect_equal(s_swap$family$outcome_coerce, "round_clip")

  s_re <- texanshootR:::apply_model_form(
    spec, list(kind = "random_effect", group = "grp"))
  expect_equal(s_re$family$fitter, "glmm")
  expect_equal(s_re$family$group, "grp")
})

test_that("apply_model_form leaves spec$family alone for predictor wraps", {
  spec <- texanshootR:::new_spec(c("x1"))
  spec$family <- list(fitter = "lm")
  s_poly <- texanshootR:::apply_model_form(
    spec, list(kind = "polynomial", var = "x1", degree = 2L))
  expect_equal(s_poly$family$fitter, "lm")
  s_spl <- texanshootR:::apply_model_form(
    spec, list(kind = "spline", var = "x1", df = 3L))
  expect_equal(s_spl$family$fitter, "lm")
})

test_that("materialise_spec wraps polynomial / spline predictors in RHS", {
  d <- make_phase2_df()
  spec <- texanshootR:::new_spec(c("x1", "x2"))
  spec$model_form <- list(kind = "polynomial", var = "x1", degree = 2L)
  mat <- texanshootR:::materialise_spec(d, "y", spec)
  expect_match(mat$formula, "poly(x1, 2)", fixed = TRUE)

  spec$model_form <- list(kind = "spline", var = "x2", df = 3L)
  mat <- texanshootR:::materialise_spec(d, "y", spec)
  expect_match(mat$formula, "splines::ns(x2, df = 3)", fixed = TRUE)
})

test_that("fit_rlm returns a well-formed result row", {
  d <- make_phase2_df()
  spec <- texanshootR:::new_spec(c("x1", "x2"))
  spec$subset_full <- "x1,x2"
  spec$family <- list(fitter = "rlm")
  r <- texanshootR:::fit_spec(d, "y", spec)
  expect_type(r, "list")
  expect_equal(r$family, "rlm:huber")
  expect_true(is.finite(r$p_value))
  expect_true(is.finite(r$r_squared))
  expect_true(is.finite(r$aic))
  expect_equal(r$n, nrow(d))
})

test_that("describe_spec renders each model_form sub-kind", {
  spec_poly <- list(
    formula = "y ~ poly(x1, 2) + x2",
    model_form = list(kind = "polynomial", var = "x1", degree = 2L)
  )
  expect_match(texanshootR:::describe_spec(spec_poly),
               "allowing for a quadratic trend in x1")
  expect_match(texanshootR:::describe_spec(spec_poly),
               "y ~ x1 + x2", fixed = TRUE)

  spec_spl <- list(
    formula = "y ~ splines::ns(x1, df = 3) + x2",
    model_form = list(kind = "spline", var = "x1", df = 3L)
  )
  expect_match(texanshootR:::describe_spec(spec_spl),
               "modelling x1 flexibly via natural splines")

  spec_rob <- list(formula = "y ~ x1", model_form = list(kind = "robust"))
  expect_match(texanshootR:::describe_spec(spec_rob),
               "using a robust M-estimator")

  spec_re  <- list(formula = "y ~ x1 + (1 | grp)",
                   model_form = list(kind = "random_effect", group = "grp"))
  expect_match(texanshootR:::describe_spec(spec_re),
               "with a random intercept for grp")

  spec_swap <- list(
    formula = "y ~ x1",
    model_form = list(kind = "family_swap", family = "binomial",
                      link = "logit",
                      outcome_coerce = "median_dichotomize"))
  expect_match(texanshootR:::describe_spec(spec_swap),
               "logistic GLM after dichotomising")
})

test_that("methods_summary chains restriction + outcome + model_form", {
  spec <- list(
    formula = "y ~ poly(x1, 2)",
    restriction = list(kind = "outcome_iqr"),
    outcome_construction = list(kind = "z_within", factor = "grp"),
    model_form = list(kind = "polynomial", var = "x1", degree = 2L)
  )
  summary <- texanshootR:::methods_summary(spec)
  expect_match(summary, "winsorising")
  expect_match(summary, "z-scoring")
  expect_match(summary, "quadratic trend")
})

test_that("apply_model_form with NULL is a no-op", {
  spec <- texanshootR:::new_spec("x1")
  spec$family <- list(fitter = "lm")
  expect_identical(texanshootR:::apply_model_form(spec, NULL), spec)
})
