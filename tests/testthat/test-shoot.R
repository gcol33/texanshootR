test_that("shoot(depth='demo') runs without theatre", {
  withr::local_seed(42)
  run <- shoot(mtcars, depth = "demo")
  expect_s3_class(run, "tx_run")
  expect_equal(run$spec_count, 1L)
})

test_that("shoot() returns a usable tx_run with theatre disabled", {
  withr::local_seed(42)
  run <- shoot(mtcars, theatrical = FALSE, budget = 2, escalate = FALSE)
  expect_s3_class(run, "tx_run")
  expect_gt(run$spec_count, 0L)
})

test_that("shoot() persists a privacy-stripped run record", {
  withr::local_seed(42)
  run <- shoot(mtcars, theatrical = FALSE, budget = 1, escalate = FALSE)
  recs <- texanshootR:::recent_run_records(1)
  expect_length(recs, 1L)
  rec <- recs[[1]]
  expect_null(rec[["df"]])    # no raw frame (use [[ to defeat $ partial match)
  expect_false(is.null(rec$df_meta))
  expect_true(!is.null(rec$df_meta$summary))
})

test_that("seed makes runs reproducible", {
  r1 <- shoot(mtcars, theatrical = FALSE, budget = 1, escalate = FALSE,
              seed = 123)
  r2 <- shoot(mtcars, theatrical = FALSE, budget = 1, escalate = FALSE,
              seed = 123)
  expect_equal(r1$grid_hash, r2$grid_hash)
})
