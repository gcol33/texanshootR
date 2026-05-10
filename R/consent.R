# First-write consent prompt. CRAN-policy compliant: no writes to user
# directories without an interactive prompt or an explicit opt-in via
# `options(texanshootR.save_enabled = TRUE)` (which is the default but
# implies the user accepted the package by installing it; we still
# prompt the first time).

consent_ok <- function(d) {
  if (!isTRUE(opt("texanshootR.save_enabled"))) return(FALSE)

  # If a meta.json already exists at d, consent is implied.
  if (file.exists(file.path(d, "meta.json"))) return(TRUE)

  # Non-interactive sessions require an explicit opt-in *with*
  # `texanshootR.consent = TRUE`; otherwise we don't write.
  if (!interactive()) {
    return(isTRUE(getOption("texanshootR.consent", FALSE)))
  }

  # Honor an explicit consent option even in interactive sessions.
  if (isTRUE(getOption("texanshootR.consent", FALSE))) return(TRUE)

  prompt <- paste0(
    "texanshootR would like to persist career data to:\n  ", d,
    "\nAllow? [y/N]: "
  )
  ans <- tolower(trimws(readline(prompt)))
  isTRUE(ans %in% c("y", "yes"))
}
