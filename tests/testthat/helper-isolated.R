# Sandbox every test from the user's real save dir.
#
# This file is sourced by `pkgload::load_all(helpers = TRUE)`. testthat
# triggers that call from inside `test_files_setup_env()` as part of
# setting up a test run; devtools triggers it independently when the
# user is just iterating interactively. Both paths sourcing the same
# helper means a naive top-level `options(...)` would leak the test
# sandbox (notably `texanshootR.animations = FALSE`) into the
# interactive session and silently disable the TUI animation.
#
# We can't gate on `Sys.getenv("TESTTHAT") == "true"` here: testthat
# only sets that env var *after* load_all returns. By that point the
# helper has already been sourced and won't be sourced again. So we
# detect the test path the only way it's actually distinguishable at
# source time -- by looking up the call stack for testthat's test
# entry points.
in_test_setup <- function() {
  calls <- as.character(sys.calls())
  any(grepl("^(testthat::)?(test_local|test_dir|test_files|test_files_serial|test_files_setup_env)\\b",
            calls))
}

if (in_test_setup()) {
  .tx_test_dir <- file.path(tempdir(), paste0("tx-test-",
                                              as.integer(Sys.time())))
  dir.create(.tx_test_dir, showWarnings = FALSE, recursive = TRUE)
  options(
    texanshootR.save_dir     = .tx_test_dir,
    texanshootR.save_enabled = TRUE,
    texanshootR.consent      = TRUE,
    texanshootR.animations   = FALSE,
    texanshootR.quiet        = TRUE,
    texanshootR.life_events  = FALSE
  )
}
