# Package environment used as a cache for the message and event
# registries, the achievement / cosmetic registries, and a few derived
# lookups. Populated by `.onLoad`.
.tx <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  set_opt_defaults()

  # Lazy: heavy registry parsing happens on first use, not at load.
  # We just initialise empty slots here so getter helpers can dispatch.
  .tx$messages     <- NULL
  .tx$events       <- NULL
  .tx$achievements <- NULL
  .tx$cosmetics    <- NULL
  .tx$mascot_frames <- NULL

  invisible()
}

.onAttach <- function(libname, pkgname) {
  if (!interactive()) return(invisible())
  if (isTRUE(opt("texanshootR.quiet"))) return(invisible())
  if (Sys.getenv("R_TESTS") != "") return(invisible())
  banner_maybe_print()
  invisible()
}
