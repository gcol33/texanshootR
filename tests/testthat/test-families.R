test_that("fit_spec dispatches lm by default", {
  spec <- texanshootR:::new_spec(c("wt", "hp"))
  spec$subset_full <- "wt,hp,cyl"
  r <- texanshootR:::fit_spec(mtcars, "mpg", spec)
  expect_type(r, "list")
  expect_equal(r$family, "lm")
  expect_true(is.numeric(r$r_squared) && r$r_squared > 0)
  expect_true(is.numeric(r$p_value) && r$p_value >= 0 && r$p_value <= 1)
})

test_that("fit_spec dispatches glm when spec asks for it", {
  spec <- texanshootR:::new_spec(c("wt", "hp"),
                                  family = list(fitter = "glm",
                                                 family = "gaussian",
                                                 link   = "identity"))
  spec$subset_full <- "wt,hp,cyl"
  r <- texanshootR:::fit_spec(mtcars, "mpg", spec)
  expect_type(r, "list")
  expect_match(r$family, "^glm:gaussian:identity")
  expect_true(is.numeric(r$r_squared))
  expect_true(is.numeric(r$p_value))
  expect_true(is.list(r$family_meta))
  expect_equal(r$family_meta$family, "gaussian")
})

test_that("fit_glm handles binary outcome with logit", {
  d <- mtcars
  spec <- texanshootR:::new_spec(c("wt", "hp"),
                                  family = list(fitter = "glm",
                                                 family = "binomial",
                                                 link   = "logit"))
  spec$subset_full <- "wt,hp"
  r <- texanshootR:::fit_spec(d, "am", spec)  # am is 0/1
  expect_type(r, "list")
  expect_match(r$family, "^glm:binomial:logit")
})

test_that("fit_glm handles count outcome with poisson log", {
  d <- mtcars
  d$cyl <- as.integer(d$cyl)
  spec <- texanshootR:::new_spec(c("wt", "hp"),
                                  family = list(fitter = "glm",
                                                 family = "poisson",
                                                 link   = "log"))
  spec$subset_full <- "wt,hp"
  r <- texanshootR:::fit_spec(d, "cyl", spec)
  expect_type(r, "list")
  expect_match(r$family, "^glm:poisson:log")
})

test_that("fit_glm rejects out-of-domain outcomes (poisson on negatives)", {
  d <- mtcars; d$neg <- -seq_len(nrow(d))
  spec <- texanshootR:::new_spec(c("wt", "hp"),
                                  family = list(fitter = "glm",
                                                 family = "poisson",
                                                 link   = "log"))
  spec$subset_full <- "wt,hp"
  expect_null(texanshootR:::fit_spec(d, "neg", spec))
})

test_that("fit_glm with median_dichotomize coerces a continuous outcome", {
  d <- mtcars
  spec <- texanshootR:::new_spec(c("wt", "hp"),
                                  family = list(fitter = "glm",
                                                 family = "binomial",
                                                 link   = "logit",
                                                 outcome_coerce =
                                                   "median_dichotomize"))
  spec$subset_full <- "wt,hp"
  r <- texanshootR:::fit_spec(d, "mpg", spec)
  expect_type(r, "list")
  expect_match(r$family, "median_dichotomize")
})

test_that("available_families gates families by career tier", {
  expect_equal(texanshootR:::available_families("Junior Researcher"), "lm")
  expect_setequal(texanshootR:::available_families("Postdoc"),
                   c("lm", "glm", "cor"))
  expect_setequal(texanshootR:::available_families("Senior Scientist"),
                   c("lm", "glm", "cor", "wls", "gam"))
  expect_setequal(texanshootR:::available_families("PI"),
                   c("lm", "glm", "cor", "wls", "gam", "glmm", "sem"))
})

test_that("select_family at Junior is always lm", {
  set.seed(7)
  out <- replicate(20, texanshootR:::select_family(
    "Junior Researcher", outcome = mtcars$mpg), simplify = FALSE)
  fitters <- vapply(out, function(x) x$fitter, character(1))
  expect_true(all(fitters == "lm"))
})

test_that("select_family at PI eventually returns glm", {
  set.seed(7)
  out <- replicate(50, texanshootR:::select_family(
    "PI", outcome = mtcars$mpg), simplify = FALSE)
  fitters <- vapply(out, function(x) x$fitter, character(1))
  expect_true(any(fitters == "glm"))
})

test_that("select_family in escalation prefers glm and may flail", {
  set.seed(11)
  out <- replicate(50, texanshootR:::select_family(
    "Postdoc", escalating = TRUE, outcome = mtcars$mpg), simplify = FALSE)
  fitters <- vapply(out, function(x) x$fitter, character(1))
  expect_gt(mean(fitters == "glm"), 0.6)

  glms <- Filter(function(x) x$fitter == "glm", out)
  flails <- vapply(glms, function(x) !is.null(x$outcome_coerce), logical(1))
  expect_true(any(flails))  # at least one flail across the 50 rolls
})

test_that("fit_gam fits a smooth-term model on a continuous outcome", {
  spec <- texanshootR:::new_spec(c("wt", "hp"),
                                  family = list(fitter = "gam"))
  spec$subset_full <- "wt,hp,cyl"
  r <- texanshootR:::fit_spec(mtcars, "mpg", spec)
  expect_type(r, "list")
  expect_match(r$family, "^gam:gaussian:identity")
  expect_true(grepl("s\\(wt", r$formula))
  expect_true(is.numeric(r$r_squared) && r$r_squared > 0)
  expect_true(is.numeric(r$p_value) && r$p_value >= 0 && r$p_value <= 1)
  expect_equal(r$family_meta$family, "gaussian")
  expect_true(is.numeric(r$family_meta$lambda) && r$family_meta$lambda > 0)
  expect_true(is.numeric(r$family_meta$edf) && r$family_meta$edf > 0)
})

test_that("fit_gam falls back to parametric terms for low-cardinality predictors", {
  spec <- texanshootR:::new_spec(c("am", "wt"),
                                  family = list(fitter = "gam"))
  spec$subset_full <- "am,wt"
  r <- texanshootR:::fit_spec(mtcars, "mpg", spec)
  expect_type(r, "list")
  # am has only two levels — must NOT be wrapped in s().
  expect_false(grepl("s\\(am", r$formula))
  expect_true(grepl("s\\(wt", r$formula))
  expect_setequal(r$family_meta$smoothed, "wt")
})

test_that("fit_gam recovers a known smooth signal far better than lm", {
  set.seed(31)
  n <- 200L
  # cos(x) on [0, 2*pi] has zero linear trend by symmetry, so lm
  # should explain ~no variance while a smooth term recovers most of it.
  x <- sort(stats::runif(n, 0, 2 * pi))
  y <- cos(x) + stats::rnorm(n, sd = 0.2)
  d <- data.frame(x = x, y = y)
  spec_gam <- texanshootR:::new_spec("x",
                                      family = list(fitter = "gam"))
  spec_gam$subset_full <- "x"
  r_gam <- texanshootR:::fit_spec(d, "y", spec_gam)
  r_lm  <- texanshootR:::fit_spec(d, "y",
                                   texanshootR:::new_spec("x"))
  expect_gt(r_gam$r_squared, 0.80)
  expect_lt(r_lm$r_squared, 0.10)
})

test_that("fit_glmm fits a random-intercept model when a grouping factor exists", {
  d <- mtcars
  d$gear_g <- factor(d$gear)
  spec <- texanshootR:::new_spec(c("wt", "hp"),
                                  family = list(fitter = "glmm"))
  spec$subset_full <- "wt,hp,cyl,gear_g"
  r <- texanshootR:::fit_spec(d, "mpg", spec)
  expect_type(r, "list")
  expect_match(r$family, "^glmm:gaussian:identity")
  expect_true(grepl("\\(1 \\|", r$formula))
  expect_true(is.numeric(r$r_squared) && r$r_squared >= 0 && r$r_squared <= 1)
  expect_true(is.numeric(r$p_value) && r$p_value >= 0 && r$p_value <= 1)
  expect_true(!is.null(r$family_meta$group))
  expect_true(is.numeric(r$family_meta$theta) && r$family_meta$theta >= 0)
})

test_that("fit_glmm recovers a meaningful between-group variance", {
  set.seed(47)
  G <- 12L; per <- 15L
  group <- factor(rep(seq_len(G), each = per))
  b <- stats::rnorm(G, sd = 2.0)
  x <- stats::rnorm(G * per)
  y <- 1 + 0.5 * x + b[as.integer(group)] + stats::rnorm(G * per, sd = 0.5)
  d <- data.frame(y = y, x = x, group = group)
  spec <- texanshootR:::new_spec("x",
                                  family = list(fitter = "glmm"))
  spec$subset_full <- "x,group"
  r <- texanshootR:::fit_spec(d, "y", spec)
  expect_type(r, "list")
  # Strong group signal should drive a small p-value.
  expect_lt(r$p_value, 0.001)
  # sigma_b^2 should land near 4 (sd = 2) — not exact, just same order.
  expect_gt(r$family_meta$sigma_b2, 1)
  expect_lt(r$family_meta$sigma_b2, 16)
})

test_that("fit_glmm returns NULL when no grouping factor is available", {
  d <- mtcars[, c("mpg", "wt", "hp")]
  spec <- texanshootR:::new_spec(c("wt", "hp"),
                                  family = list(fitter = "glmm"))
  spec$subset_full <- "wt,hp"
  expect_null(texanshootR:::fit_spec(d, "mpg", spec))
})

test_that("select_family at Senior may pick gam", {
  set.seed(13)
  out <- replicate(200, texanshootR:::select_family(
    "Senior Scientist", outcome = mtcars$mpg), simplify = FALSE)
  fitters <- vapply(out, function(x) x$fitter, character(1))
  expect_true(any(fitters == "gam"))
  expect_true(any(fitters == "glm"))
  expect_false(any(fitters == "glmm"))  # PI-only
})

test_that("select_family at PI may pick glmm", {
  set.seed(17)
  out <- replicate(200, texanshootR:::select_family(
    "PI", outcome = mtcars$mpg), simplify = FALSE)
  fitters <- vapply(out, function(x) x$fitter, character(1))
  expect_true(any(fitters == "glmm"))
})

test_that("fit_sem fits a mediation model and reports the Sobel p-value", {
  # Synthetic mediation: X -> M -> Y, no direct path. With a strong
  # indirect effect the Sobel p-value should be tiny.
  set.seed(91)
  n <- 200L
  x <- stats::rnorm(n)
  m <- 0.7 * x + stats::rnorm(n, sd = 0.5)
  y <- 0.6 * m + stats::rnorm(n, sd = 0.5)
  d <- data.frame(y = y, x = x, m = m)
  spec <- texanshootR:::new_spec(c("x", "m"),
                                  family = list(fitter = "sem",
                                                 model  = "mediation"))
  spec$subset_full <- "x,m"
  r <- texanshootR:::fit_spec(d, "y", spec)
  expect_type(r, "list")
  expect_equal(r$family, "sem:mediation")
  expect_match(r$formula, "via m")
  expect_lt(r$p_value, 0.001)
  # Indirect effect should land near 0.7 * 0.6 = 0.42.
  expect_gt(r$family_meta$ab, 0.2)
  expect_lt(r$family_meta$ab, 0.6)
  expect_true(is.numeric(r$family_meta$sobel_z))
})

test_that("fit_sem returns a non-significant Sobel when there is no mediation", {
  set.seed(92)
  n <- 200L
  x <- stats::rnorm(n)
  # M is independent of X, so a ~ 0 and the indirect effect is nil.
  m <- stats::rnorm(n)
  y <- 0.5 * x + stats::rnorm(n, sd = 0.5)
  d <- data.frame(y = y, x = x, m = m)
  spec <- texanshootR:::new_spec(c("x", "m"),
                                  family = list(fitter = "sem"))
  spec$subset_full <- "x,m"
  r <- texanshootR:::fit_spec(d, "y", spec)
  expect_type(r, "list")
  expect_gt(r$p_value, 0.05)
})

test_that("fit_sem requires at least two predictors", {
  spec <- texanshootR:::new_spec("wt",
                                  family = list(fitter = "sem"))
  spec$subset_full <- "wt"
  expect_null(texanshootR:::fit_spec(mtcars, "mpg", spec))
})

test_that("fit_sem accepts extra subset entries as Y-equation covariates", {
  set.seed(93)
  spec <- texanshootR:::new_spec(c("wt", "hp", "cyl"),
                                  family = list(fitter = "sem"))
  spec$subset_full <- "wt,hp,cyl"
  r <- texanshootR:::fit_spec(mtcars, "mpg", spec)
  expect_type(r, "list")
  expect_equal(r$family, "sem:mediation")
  expect_setequal(r$family_meta$controls, "cyl")
})

test_that("select_family at PI may pick sem", {
  set.seed(23)
  out <- replicate(400, texanshootR:::select_family(
    "PI", outcome = mtcars$mpg), simplify = FALSE)
  fitters <- vapply(out, function(x) x$fitter, character(1))
  expect_true(any(fitters == "sem"))
})

test_that("fit_cor reports a Pearson correlation and its p-value", {
  set.seed(101)
  spec <- texanshootR:::new_spec("wt",
                                  family = list(fitter = "cor",
                                                 method  = "pearson"))
  spec$subset_full <- "wt"
  r <- texanshootR:::fit_spec(mtcars, "mpg", spec)
  expect_type(r, "list")
  expect_equal(r$family, "cor:pearson")
  expect_true(is.numeric(r$family_meta$r))
  # wt and mpg are strongly negatively correlated.
  expect_lt(r$family_meta$r, -0.7)
  expect_lt(r$p_value, 0.001)
  expect_equal(r$r_squared, r$family_meta$r^2, tolerance = 1e-10)
})

test_that("fit_cor requires exactly one predictor", {
  spec <- texanshootR:::new_spec(c("wt", "hp"),
                                  family = list(fitter = "cor"))
  spec$subset_full <- "wt,hp"
  expect_null(texanshootR:::fit_spec(mtcars, "mpg", spec))
})

test_that("fit_cor supports rank-based methods", {
  spec <- texanshootR:::new_spec("wt",
                                  family = list(fitter = "cor",
                                                 method  = "spearman"))
  spec$subset_full <- "wt"
  r <- texanshootR:::fit_spec(mtcars, "mpg", spec)
  expect_equal(r$family, "cor:spearman")
})

test_that("fit_wls reweights a heteroscedastic regression", {
  # Build data where Var(y | x) grows with |x|. WLS should match OLS
  # qualitatively (both find the slope) but produce a different RSS
  # because the weighting changes the residual norm.
  set.seed(111)
  n <- 200L
  x <- stats::rnorm(n)
  y <- 1 + 0.8 * x + stats::rnorm(n, sd = 0.3 + abs(x))
  d <- data.frame(y = y, x = x)
  spec_wls <- texanshootR:::new_spec("x",
                                      family = list(fitter = "wls"))
  spec_wls$subset_full <- "x"
  r_wls <- texanshootR:::fit_spec(d, "y", spec_wls)
  r_lm  <- texanshootR:::fit_spec(d, "y",
                                   texanshootR:::new_spec("x"))
  expect_type(r_wls, "list")
  expect_equal(r_wls$family, "wls")
  expect_true(is.numeric(r_wls$r_squared) && r_wls$r_squared > 0)
  # The two strategies share the same significance verdict here.
  expect_lt(r_wls$p_value, 0.05)
  expect_lt(r_lm$p_value, 0.05)
})

test_that("select_family at Postdoc may pick cor", {
  set.seed(31)
  out <- replicate(500, texanshootR:::select_family(
    "Postdoc", outcome = mtcars$mpg), simplify = FALSE)
  fitters <- vapply(out, function(x) x$fitter, character(1))
  expect_true(any(fitters == "cor"))
  expect_false(any(fitters == "wls"))  # Senior+
})

test_that("select_family at Senior may pick wls", {
  set.seed(37)
  out <- replicate(500, texanshootR:::select_family(
    "Senior Scientist", outcome = mtcars$mpg), simplify = FALSE)
  fitters <- vapply(out, function(x) x$fitter, character(1))
  expect_true(any(fitters == "wls"))
})

test_that("classify_outcome distinguishes the four shapes", {
  expect_equal(texanshootR:::classify_outcome(c(0, 1, 1, 0)), "binary")
  expect_equal(texanshootR:::classify_outcome(c(0L, 2L, 5L, 3L)), "count")
  expect_equal(texanshootR:::classify_outcome(c(1.2, 3.4, 0.5)), "positive")
  expect_equal(texanshootR:::classify_outcome(c(-1, 2, 0.5)), "continuous")
  expect_equal(texanshootR:::classify_outcome(NULL), "unknown")
  expect_equal(texanshootR:::classify_outcome(letters), "unknown")
})
