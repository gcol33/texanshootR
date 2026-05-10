set_career_level <- function(level) {
  texanshootR:::ensure_save_dir()
  meta <- texanshootR:::read_meta()
  if (is.null(meta)) {
    meta <- texanshootR:::default_meta(getOption("texanshootR.save_dir"))
  }
  meta$career_level <- level
  texanshootR:::write_meta(meta)
  invisible(meta)
}

test_that("tier_index orders the four tiers as defined", {
  expect_equal(texanshootR:::tier_index("Junior Researcher"), 1L)
  expect_equal(texanshootR:::tier_index("Postdoc"), 2L)
  expect_equal(texanshootR:::tier_index("Senior Scientist"), 3L)
  expect_equal(texanshootR:::tier_index("PI"), 4L)
  expect_equal(texanshootR:::tier_index("nonsense"), 0L)
})

test_that("require_unlocked() signals tx_locked when career is too low", {
  set_career_level("Postdoc")
  err <- tryCatch(
    texanshootR:::require_unlocked("presentation"),
    tx_locked = identity
  )
  expect_s3_class(err, "tx_locked")
  expect_equal(err$fn, "presentation")
  expect_equal(err$required, "Senior Scientist")
  expect_equal(err$current, "Postdoc")
  expect_match(conditionMessage(err), "presentation\\(\\) requires:")
  expect_match(conditionMessage(err), "Senior Scientist")
  expect_match(conditionMessage(err), "Current career:")
  expect_match(conditionMessage(err), "Postdoc")
})

test_that("require_unlocked() passes through when tier is met", {
  set_career_level("Senior Scientist")
  expect_true(texanshootR:::require_unlocked("presentation"))
  expect_true(texanshootR:::require_unlocked("manuscript"))
})

test_that("require_unlocked() passes for an unregistered name", {
  expect_true(texanshootR:::require_unlocked("not_in_registry"))
})

test_that("locked-message format is the deadpan two-block layout", {
  msg <- texanshootR:::format_locked_message(
    "presentation", "Senior Scientist", "Postdoc"
  )
  expect_identical(
    msg,
    "presentation() requires:\nSenior Scientist\n\nCurrent career:\nPostdoc"
  )
})

test_that("calling a locked generator raises tx_locked", {
  set_career_level("Junior Researcher")
  expect_error(presentation(list()), class = "tx_locked")
  expect_error(manuscript(list()),   class = "tx_locked")
  expect_error(funding(list()),      class = "tx_locked")
})

test_that("progress() returns a tx_progress overview object", {
  set_career_level("Postdoc")
  obj <- expect_invisible(progress())
  expect_s3_class(obj, "tx_progress")
  expect_equal(obj$mode, "overview")
  expect_equal(obj$current, "Postdoc")
  fn_names <- vapply(obj$rows, function(r) r$fn, character(1))
  expect_setequal(fn_names, names(texanshootR:::UNLOCK_REGISTRY))
  unlocked_fns <- vapply(obj$rows, function(r) r$unlocked, logical(1))
  names(unlocked_fns) <- fn_names
  expect_true(unlocked_fns[["manuscript"]])
  expect_true(unlocked_fns[["preprint"]])
  expect_false(unlocked_fns[["presentation"]])
  expect_false(unlocked_fns[["funding"]])
  expect_true(is.numeric(obj$ach_unlocked))
  expect_true(is.numeric(obj$ach_total))
  expect_true(is.list(obj$inflight))
})

test_that("progress(\"presentation\") returns a per-function card", {
  set_career_level("Junior Researcher")
  obj <- progress("presentation")
  expect_equal(obj$mode, "function")
  expect_equal(obj$fn, "presentation")
  expect_equal(obj$required, "Senior Scientist")
  expect_false(obj$unlocked)
})

test_that("progress(\"ach_*\") returns a per-achievement card", {
  obj <- progress("ach_multiple_comparisons")
  expect_equal(obj$mode, "achievement")
  expect_equal(obj$id, "ach_multiple_comparisons")
  expect_false(obj$unlocked)
  expect_false(obj$hidden_locked)
  expect_equal(obj$detail$need, 1000L)
})

test_that("progress(\"ach_*\") for a hidden-locked achievement masks the name", {
  obj <- progress("ach_glimpse")
  expect_equal(obj$mode, "achievement")
  expect_true(obj$hidden_locked)
  expect_equal(obj$name, "???")
  expect_null(obj$detail)
})

test_that("progress(bogus) errors", {
  expect_error(progress("not_a_thing"),
                "not a gated function or achievement id")
})

test_that("in-flight progress excludes hidden achievements", {
  set_career_level("Junior Researcher")
  obj <- progress()
  ids <- vapply(obj$inflight, `[[`, character(1), "id")
  reg <- texanshootR:::load_achievement_registry()
  visibilities <- reg$visible[match(ids, reg$id)]
  expect_true(all(visibilities))
})
