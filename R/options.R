#' Package options for texanshootR
#'
#' All options are read at the time of use, so changing them takes
#' effect immediately for subsequent calls.
#'
#' \describe{
#'   \item{`texanshootR.animations`}{Logical. Enable terminal
#'     animations and ANSI cursor effects. Default: `TRUE`. Forced off
#'     in non-interactive sessions and when the terminal lacks ANSI
#'     support.}
#'   \item{`texanshootR.output_dir`}{Character or `NULL`. Default
#'     directory for output files. `NULL` falls back to
#'     [tempdir()].}
#'   \item{`texanshootR.save_enabled`}{Logical. Enable persistent
#'     career data. `FALSE` runs entirely in memory. Default: `TRUE`.}
#'   \item{`texanshootR.quiet`}{Logical. Suppress status prints from
#'     output functions. Default: `FALSE`.}
#'   \item{`texanshootR.event_rate`}{Numeric in `[0, 1]`. Per-run
#'     probability of a life-event encounter. Default: `1/6`.}
#'   \item{`texanshootR.life_events`}{Logical. Enable life-event
#'     encounters during runs. Default: `TRUE`.}
#'   \item{`texanshootR.ui_mode`}{Optional character override for the
#'     TUI rendering mode. One of `"ansi"`, `"dynamic"`, `"plain"`.
#'     Unset (the default) means auto-detect. Use this if auto-detect
#'     picks the wrong mode for your terminal (e.g., a remote RStudio
#'     session that does not report `.Platform$GUI == "RStudio"`).}
#' }
#' @name texanshootR-options
NULL

.opt_defaults <- list(
  texanshootR.animations   = TRUE,
  texanshootR.output_dir   = NULL,
  texanshootR.save_enabled = TRUE,
  texanshootR.quiet        = FALSE,
  texanshootR.event_rate   = 1 / 6,
  texanshootR.life_events  = TRUE
)

#' @keywords internal
opt <- function(name, default = .opt_defaults[[name]]) {
  v <- getOption(name, default)
  if (is.null(v)) default else v
}

#' @keywords internal
set_opt_defaults <- function() {
  for (nm in names(.opt_defaults)) {
    if (is.null(getOption(nm))) {
      args <- stats::setNames(list(.opt_defaults[[nm]]), nm)
      do.call(options, args)
    }
  }
  invisible()
}
