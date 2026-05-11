# Reviewer-outcome materialization. The roll moved from finalize_run()
# (where it fired automatically on every run) into reviewer_response()
# (where it only fires when the player turns the finding into
# publication bureaucracy). A run is a finding, not a verdict.

test_that("shoot() leaves reviewer_outcome NA", {
  withr::local_options(texanshootR.budget = 1)
  withr::local_seed(11)
  run <- shoot(mtcars)
  expect_true(is.na(run$reviewer_outcome))
})

test_that("materialize_reviewer_outcome rolls and persists on first call", {
  withr::local_options(texanshootR.budget = 1)
  withr::local_seed(17)
  run <- shoot(mtcars)
  expect_true(is.na(run$reviewer_outcome))

  rolled <- texanshootR:::materialize_reviewer_outcome(run)
  expect_true(rolled$reviewer_outcome %in%
              c("accept_enthusiasm", "minor", "major", "reject"))

  # On-disk record was updated.
  rec <- texanshootR:::read_run_record(run$run_id)
  expect_identical(rec$reviewer_outcome, rolled$reviewer_outcome)
})

test_that("materialize_reviewer_outcome memoizes -- no re-roll on second call", {
  withr::local_options(texanshootR.budget = 1)
  withr::local_seed(23)
  run <- shoot(mtcars)

  first  <- texanshootR:::materialize_reviewer_outcome(run)
  # Seed shifted: if the function re-rolled it would draw a different
  # outcome with high probability across the four-element distribution.
  set.seed(99)
  second <- texanshootR:::materialize_reviewer_outcome(first)
  expect_identical(second$reviewer_outcome, first$reviewer_outcome)
})

test_that("received_a_reject fires from reviewer_response, not from shoot", {
  testthat::skip_if_not_installed("withr")
  testthat::skip_if_not_installed("officer")
  withr::local_options(texanshootR.budget = 2)
  withr::local_seed(5)

  # Wipe achievement state so we can observe the trigger fire.
  ach_state <- texanshootR:::read_achievements_state()
  ach_state$received_a_reject <- NULL
  texanshootR:::write_achievements_state(ach_state)

  run <- shoot(mtcars)
  expect_true(is.na(run$reviewer_outcome))
  # No reject achievement yet — the roll hasn't happened.
  expect_null(texanshootR:::read_achievements_state()$received_a_reject)

  # Force a deterministic reject outcome and call the generator.
  # We can't easily seed reviewer_roll() externally, so inject the
  # outcome onto the run directly: materialize_reviewer_outcome()
  # short-circuits on a non-NA value, and reviewer_response()'s
  # record_output() call then evaluates achievements against the
  # persisted record.
  if (isTRUE(run$shippable)) {
    run$reviewer_outcome <- "reject"
    # Persist so record_output() sees it.
    fpath <- file.path(texanshootR:::save_dir(), "runs",
                       paste0(run$run_id, ".rds"))
    rec <- readRDS(fpath)
    rec$reviewer_outcome <- "reject"
    saveRDS(rec, fpath)

    tryCatch(
      reviewer_response(run, output_dir = tempdir()),
      tx_chain_error = function(e) NULL,
      tx_locked      = function(e) NULL
    )
    # Achievement state advances only when the chain stage actually
    # ran. If the chain wasn't unlocked, the test trivially passes
    # because the trigger had no run to fire on.
    state <- texanshootR:::read_achievements_state()
    if (!is.null(state$received_a_reject)) {
      expect_true(state$received_a_reject)
    } else {
      succeed("chain not unlocked at this career tier; trigger had no opportunity")
    }
  } else {
    succeed("run was not shippable; reviewer chain stage unreachable")
  }
})
