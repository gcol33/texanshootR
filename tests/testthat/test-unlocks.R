set_chain_length <- function(length_unlocked) {
  texanshootR:::ensure_save_dir()
  meta <- texanshootR:::read_meta()
  if (is.null(meta)) {
    meta <- texanshootR:::default_meta(getOption("texanshootR.save_dir"))
  }
  meta$progression$length_unlocked <- as.integer(length_unlocked)
  meta$progression$xp <-
    texanshootR:::CHAIN_XP_THRESHOLDS[as.integer(length_unlocked)]
  meta$career_level <-
    texanshootR:::career_level_for_length(length_unlocked)
  texanshootR:::write_meta(meta)
  invisible(meta)
}

clear_active_chain <- function() {
  meta <- texanshootR:::read_meta()
  if (is.null(meta)) return(invisible())
  meta$active_chain <- NULL
  texanshootR:::write_meta(meta)
}

test_that("CHAIN_STAGES carries six ordered stages", {
  expect_equal(length(texanshootR:::CHAIN_STAGES), 6L)
  expect_equal(texanshootR:::CHAIN_STAGES[[1]], "abstract")
  expect_equal(texanshootR:::CHAIN_STAGES[[6]], "funding")
})

test_that("length_unlocked_for_xp climbs as XP crosses thresholds", {
  expect_equal(texanshootR:::length_unlocked_for_xp(0L),   1L)
  expect_equal(texanshootR:::length_unlocked_for_xp(4L),   1L)
  expect_equal(texanshootR:::length_unlocked_for_xp(5L),   2L)
  expect_equal(texanshootR:::length_unlocked_for_xp(15L),  3L)
  expect_equal(texanshootR:::length_unlocked_for_xp(200L), 6L)
})

test_that("career_level_for_length maps to canonical tier labels", {
  expect_equal(texanshootR:::career_level_for_length(1L),
               "Junior Researcher")
  expect_equal(texanshootR:::career_level_for_length(2L), "Postdoc")
  expect_equal(texanshootR:::career_level_for_length(4L),
               "Senior Scientist")
  expect_equal(texanshootR:::career_level_for_length(6L), "PI")
})

test_that("is_unlocked tracks chain length", {
  set_chain_length(1L)
  expect_true(texanshootR:::is_unlocked("abstract"))
  expect_false(texanshootR:::is_unlocked("manuscript"))
  set_chain_length(2L)
  expect_true(texanshootR:::is_unlocked("manuscript"))
  expect_false(texanshootR:::is_unlocked("presentation"))
  set_chain_length(6L)
  expect_true(texanshootR:::is_unlocked("funding"))
})

test_that("require_chain_stage signals not_unlocked when stage above XP", {
  set_chain_length(1L)
  clear_active_chain()
  err <- tryCatch(
    texanshootR:::require_chain_stage("manuscript", list(run_id = "x")),
    tx_chain_error = identity
  )
  expect_s3_class(err, "tx_chain_error")
  expect_equal(err$reason, "not_unlocked")
})

test_that("require_chain_stage signals no_active_chain when none open", {
  set_chain_length(6L)
  clear_active_chain()
  err <- tryCatch(
    texanshootR:::require_chain_stage("abstract", list(run_id = "x")),
    tx_chain_error = identity
  )
  expect_s3_class(err, "tx_chain_error")
  expect_equal(err$reason, "no_active_chain")
})

test_that("require_chain_stage signals wrong_run when run id mismatches", {
  set_chain_length(2L)
  meta <- texanshootR:::read_meta()
  meta <- texanshootR:::open_chain(meta, "RUN_A")
  texanshootR:::write_meta(meta)
  err <- tryCatch(
    texanshootR:::require_chain_stage("abstract", list(run_id = "RUN_B")),
    tx_chain_error = identity
  )
  expect_s3_class(err, "tx_chain_error")
  expect_equal(err$reason, "wrong_run")
})

test_that("require_chain_stage signals wrong_stage when out of order", {
  set_chain_length(2L)
  meta <- texanshootR:::read_meta()
  meta <- texanshootR:::open_chain(meta, "RUN_A")
  texanshootR:::write_meta(meta)
  err <- tryCatch(
    texanshootR:::require_chain_stage("manuscript", list(run_id = "RUN_A")),
    tx_chain_error = identity
  )
  expect_s3_class(err, "tx_chain_error")
  expect_equal(err$reason, "wrong_stage")
})

test_that("require_chain_stage passes when stage matches active chain", {
  set_chain_length(2L)
  meta <- texanshootR:::read_meta()
  meta <- texanshootR:::open_chain(meta, "RUN_A")
  texanshootR:::write_meta(meta)
  res <- texanshootR:::require_chain_stage("abstract",
                                            list(run_id = "RUN_A"))
  expect_true(res$ok)
})

test_that("advance_chain_after_stage moves the chain forward and awards XP", {
  set_chain_length(2L)
  meta <- texanshootR:::read_meta()
  meta <- texanshootR:::open_chain(meta, "RUN_A")
  texanshootR:::write_meta(meta)
  prog_before <- texanshootR:::progression_of(texanshootR:::read_meta())

  texanshootR:::advance_chain_after_stage("abstract")
  meta_after <- texanshootR:::read_meta()
  prog_after <- texanshootR:::progression_of(meta_after)
  expect_gt(prog_after$xp, prog_before$xp)
  expect_equal(meta_after$active_chain$stage_idx, 2L)
})

test_that("completing the unlocked prefix closes the chain", {
  set_chain_length(1L)
  meta <- texanshootR:::read_meta()
  meta <- texanshootR:::open_chain(meta, "RUN_A")
  texanshootR:::write_meta(meta)
  texanshootR:::advance_chain_after_stage("abstract")
  m2 <- texanshootR:::read_meta()
  expect_null(m2$active_chain)
  prog <- texanshootR:::progression_of(m2)
  expect_equal(prog$chains_completed, 1L)
})

test_that("progress() returns a tx_progress overview with chain fields", {
  set_chain_length(2L)
  obj <- expect_invisible(progress())
  expect_s3_class(obj, "tx_progress")
  expect_equal(obj$mode, "overview")
  expect_equal(obj$chain_length, 2L)
  expect_true(is.numeric(obj$xp))
})
