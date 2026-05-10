test_that("shoot(depth='demo') runs the single-fit smoke path", {
  withr::local_seed(42)
  run <- shoot(mtcars, depth = "demo")
  expect_s3_class(run, "tx_run")
  expect_equal(run$spec_count, 1L)
})

test_that("shoot() returns a usable tx_run", {
  withr::local_options(texanshootR.budget = 2)
  withr::local_seed(42)
  run <- shoot(mtcars, escalate = FALSE)
  expect_s3_class(run, "tx_run")
  expect_gt(run$spec_count, 0L)
})

test_that("shoot() persists a privacy-stripped run record", {
  withr::local_options(texanshootR.budget = 1)
  withr::local_seed(42)
  run <- shoot(mtcars, escalate = FALSE)
  recs <- texanshootR:::recent_run_records(1)
  expect_length(recs, 1L)
  rec <- recs[[1]]
  expect_null(rec[["df"]])    # no raw frame (use [[ to defeat $ partial match)
  expect_false(is.null(rec$df_meta))
  expect_true(!is.null(rec$df_meta$summary))
})

test_that("seed makes runs reproducible", {
  withr::local_options(texanshootR.budget = 1)
  r1 <- shoot(mtcars, escalate = FALSE, seed = 123)
  r2 <- shoot(mtcars, escalate = FALSE, seed = 123)
  expect_equal(r1$grid_hash, r2$grid_hash)
})
