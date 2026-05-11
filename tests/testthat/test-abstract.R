test_that("abstract() refuses when no chain is active", {
  meta <- texanshootR:::read_meta() %||% texanshootR:::default_meta(getOption("texanshootR.save_dir"))
  meta$active_chain <- NULL
  texanshootR:::write_meta(meta)
  fake_run <- list(run_id = "TEST",
                   highlighted_spec = list(formula = "y ~ x",
                                            r_squared = 0.5,
                                            p_value = 0.04),
                   spec_count = 10L)
  err <- tryCatch(abstract(fake_run), tx_chain_error = identity)
  expect_s3_class(err, "tx_chain_error")
  expect_equal(err$reason, "no_active_chain")
})

test_that("abstract() writes a .txt and advances the chain when stage matches", {
  meta <- texanshootR:::read_meta() %||% texanshootR:::default_meta(getOption("texanshootR.save_dir"))
  meta$progression$length_unlocked <- 2L
  meta$progression$xp <- texanshootR:::CHAIN_XP_THRESHOLDS[2L]
  meta <- texanshootR:::open_chain(meta, "TEST_RUN")
  texanshootR:::write_meta(meta)

  # Need a run record for record_output() to update.
  rec <- list(run_id = "TEST_RUN", outputs_generated = character(),
              outputs_generated_files = character())
  d <- texanshootR:::ensure_save_dir()
  saveRDS(rec, file.path(d, "runs", "TEST_RUN.rds"))

  out_dir <- tempfile("abs-out-")
  fake_run <- list(run_id = "TEST_RUN",
                   highlighted_spec = list(formula = "mpg ~ wt",
                                            r_squared = 0.75,
                                            p_value = 0.001),
                   spec_count = 42L)
  path <- abstract(fake_run, output_dir = out_dir, force = TRUE)
  expect_true(file.exists(path))
  txt <- readLines(path)
  expect_true(any(grepl("Abstract", txt)))

  meta2 <- texanshootR:::read_meta()
  # Length-2 chain: abstract landed, manuscript is now due.
  expect_equal(meta2$active_chain$stage_idx, 2L)
  expect_true("abstract" %in% meta2$active_chain$completed_stages)
})
