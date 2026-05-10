# Sandbox every test from the user's real save dir.
# This file is sourced by testthat before any test file. Setting
# options at top level is fine — testthat runs each test in the same
# process, but our save_dir is hermetic.
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
