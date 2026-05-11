test_that("shoot(depth='demo') runs the single-fit smoke path", {
  withr::local_seed(42)
  run <- shoot(mtcars, depth = "demo")
  expect_s3_class(run, "tx_run")
  expect_equal(run$spec_count, 1L)
})

test_that("shoot() returns a usable tx_run", {
  withr::local_options(texanshootR.budget = 2)
  withr::local_seed(42)
  run <- shoot(mtcars)
  expect_s3_class(run, "tx_run")
  expect_gt(run$spec_count, 0L)
})

test_that("shoot() persists a privacy-stripped run record", {
  withr::local_options(texanshootR.budget = 1)
  withr::local_seed(42)
  run <- shoot(mtcars)
  recs <- texanshootR:::recent_run_records(1)
  expect_length(recs, 1L)
  rec <- recs[[1]]
  expect_null(rec[["df"]])    # no raw frame (use [[ to defeat $ partial match)
  expect_false(is.null(rec$df_meta))
  expect_true(!is.null(rec$df_meta$summary))
})

test_that("seed makes runs reproducible", {
  withr::local_options(texanshootR.budget = 1)
  r1 <- shoot(mtcars, seed = 123)
  r2 <- shoot(mtcars, seed = 123)
  expect_equal(r1$grid_hash, r2$grid_hash)
})

test_that("tactical_pause rejects unknown +commands softly", {
  withr::local_options(texanshootR.budget = 2)
  withr::local_seed(13)
  run <- shoot(mtcars,
               prompt_fn = function(prompt) "+not_a_modifier")
  expect_s3_class(run, "tx_run")
  expect_length(run$modifiers_used %||% character(), 0L)
})

test_that("typing +derived_metrics mid-run engages the escalation phase", {
  withr::local_options(texanshootR.budget = 2)
  withr::local_seed(7)
  # Reply with +derived_metrics to whichever tactical-pause window fires
  # first; later windows return "" (skip).
  responses <- c("+derived_metrics")
  i <- 0L
  pf <- function(prompt) {
    i <<- i + 1L
    if (i <= length(responses)) responses[i] else ""
  }
  run <- shoot(mtcars, prompt_fn = pf)
  expect_s3_class(run, "tx_run")
  expect_true("derived_metrics" %in% (run$modifiers_used %||% character()))
})
