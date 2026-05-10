# Startup banner. Fires with low probability on .onAttach. Suppressed
# in non-interactive sessions, in tests, and when quiet mode is on.

banner_maybe_print <- function() {
  if (runif(1) > 1 / 8) return(invisible())
  flavor <- tryCatch(daily_flavor(), error = function(e) NULL)
  draw <- tryCatch(
    select_message(phase = "banner", tag_boost = flavor$tag_boost),
    error = function(e) NULL
  )
  if (is.null(draw)) return(invisible())
  ver <- tryCatch(as.character(utils::packageVersion("texanshootR")),
                  error = function(e) "0.1.0")
  packageStartupMessage(paste0("texanshootR ", ver,
                                if (!is.null(flavor)) paste0(" - ", flavor$label) else ""))
  packageStartupMessage(draw$text)
  invisible()
}
