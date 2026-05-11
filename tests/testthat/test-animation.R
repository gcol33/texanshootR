# End-to-end tests for the dynamic single-line render.
#
# Each test:
#   - Forces dynamic mode (texanshootR.ui_mode = "dynamic") and spoofs
#     interactive() so shoot() actually opens a UI session under
#     Rscript / testthat.
#   - Replaces dyn_render with a logger that captures every composed
#     line. The bytes never hit stdout so the testthat output stays
#     clean; we assert on the captured trace.
#   - Runs shoot() to completion and inspects the trace.
#
# Why we mock interactive(): ansi_supported() and dynamic_tty_supported()
# both short-circuit on `!interactive()`. With Rscript that's FALSE and
# shoot() would skip the UI entirely. We have to patch the binding.

with_dynamic_ui <- function(code) {
  # Force the UI-activation gate to TRUE so shoot()'s open_tui branch
  # passes under Rscript / testthat. `base::interactive` itself has a
  # locked binding; we route every UI check through texanshootR's own
  # `is_interactive_ui` for exactly this reason.
  orig_gate <- texanshootR:::is_interactive_ui
  assignInNamespace("is_interactive_ui", function() TRUE,
                    ns = "texanshootR")
  on.exit(assignInNamespace("is_interactive_ui", orig_gate,
                            ns = "texanshootR"), add = TRUE)

  # Capture every composed status line dyn_render would have emitted.
  log_env <- new.env(parent = emptyenv())
  log_env$lines <- character()
  orig_render <- texanshootR:::dyn_render
  assignInNamespace("dyn_render", function(state) {
    log_env$lines <- c(log_env$lines, texanshootR:::dyn_compose(state))
    invisible()
  }, ns = "texanshootR")
  on.exit(assignInNamespace("dyn_render", orig_render, ns = "texanshootR"),
          add = TRUE)

  force(code)
  log_env$lines
}

face_glyph_of <- function(line) {
  m <- regmatches(line, regexpr("^\\([^)]*\\)", line))
  if (length(m) == 0L) NA_character_ else m
}

test_that("shoot() walks the face ladder end-to-end and flashes resolved", {
  testthat::skip_if_not_installed("withr")
  withr::local_options(
    texanshootR.animations = TRUE,
    texanshootR.ui_mode    = "dynamic",
    texanshootR.budget     = 4L,
    texanshootR.quiet      = TRUE,
    texanshootR.consent    = TRUE,
    texanshootR.life_events = FALSE
  )

  lines <- with_dynamic_ui(invisible(shoot(mtcars, seed = 1)))

  expect_gt(length(lines), 0L)

  faces <- unique(stats::na.omit(vapply(lines, face_glyph_of, character(1))))

  # The progress ladder should cover at least three states by the time
  # progress runs from 0 to 1 with a 4-second budget. We don't pin all
  # six because random per-fit timing can occasionally clip the ladder
  # at one end, but three is the floor below which something is wrong.
  expect_gte(length(faces), 3L)

  # The very last rendered face must be `resolved` (` -_-`) because the
  # final-frame flash fires when best_p <= 0.05, and mtcars hits that
  # almost immediately.
  last_face <- tail(stats::na.omit(vapply(lines, face_glyph_of, character(1))), 1)
  expect_equal(unname(last_face), "( -_-)")

  # The progress bar must reach 100% on its last update -- the loop
  # keeps animating after the spec cap is hit so the bar fills the
  # full budget regardless of how fast fits finish.
  pct <- regmatches(lines, regexpr("\\d{1,3}%", lines))
  pct <- as.integer(sub("%", "", pct))
  expect_equal(max(pct, na.rm = TRUE), 100L)
})

test_that("shoot() auto-generates output files when flagged", {
  testthat::skip_if_not_installed("withr")
  out_dir <- tempfile("tx_auto_outputs_")
  dir.create(out_dir, recursive = TRUE)
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)

  withr::local_options(
    texanshootR.animations = FALSE,   # no UI work needed for this assertion
    texanshootR.budget     = 2L,
    texanshootR.quiet      = TRUE,
    texanshootR.consent    = TRUE,
    texanshootR.life_events = FALSE,
    texanshootR.output_dir = out_dir
  )

  run <- shoot(mtcars, seed = 1, abstract = TRUE)
  expect_s3_class(run, "tx_run")
  # If the run was shippable, the abstract file should have landed.
  # If it wasn't, the flag should have been a silent no-op.
  if (isTRUE(run$shippable)) {
    expect_true(any(grepl("abstract", list.files(out_dir))))
    expect_true("abstract" %in% (run$outputs_generated %||% character()))
  } else {
    expect_length(list.files(out_dir), 0L)
  }
})

test_that("powerpoint alias maps to presentation flag", {
  testthat::skip_if_not_installed("withr")
  testthat::skip_if_not_installed("officer")
  testthat::skip_if_not_installed("rmarkdown")
  out_dir <- tempfile("tx_alias_")
  dir.create(out_dir, recursive = TRUE)
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)

  withr::local_options(
    texanshootR.animations = FALSE,
    texanshootR.budget     = 2L,
    texanshootR.quiet      = TRUE,
    texanshootR.consent    = TRUE,
    texanshootR.life_events = FALSE,
    texanshootR.output_dir = out_dir
  )

  # Force a generous chain unlock so all four prefix stages are
  # reachable for this test regardless of prior persisted state.
  meta <- texanshootR:::read_meta() %||% list()
  meta$progression <- list(xp = 999L, length_unlocked = 3L,
                            chains_completed = 0L, chains_broken = 0L)
  meta$career_level <- "Postdoc"
  texanshootR:::write_meta(meta)

  run <- shoot(mtcars, seed = 1, powerpoint = TRUE)
  expect_s3_class(run, "tx_run")
  if (isTRUE(run$shippable)) {
    # Chain order: abstract -> manuscript -> presentation. All three
    # should have been generated to reach the powerpoint stage.
    expect_setequal(run$outputs_generated,
                    c("abstract", "manuscript", "presentation"))
  }
})
