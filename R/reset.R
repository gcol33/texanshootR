#' Reset persisted texanshootR state
#'
#' These functions remove pieces of the persistent save written under
#' [tools::R_user_dir()]. They prompt for confirmation in interactive
#' sessions; pass `force = TRUE` to suppress the prompt. Non-interactive
#' sessions require `force = TRUE`.
#'
#' @param force Logical. Skip the confirmation prompt.
#' @return `TRUE` invisibly on success, `FALSE` if cancelled.
#' @name reset
NULL

confirm <- function(prompt, force) {
  if (isTRUE(force)) return(TRUE)
  if (!interactive()) {
    stop("Refusing to reset state in a non-interactive session ",
         "without `force = TRUE`.", call. = FALSE)
  }
  ans <- tolower(trimws(readline(prompt)))
  isTRUE(ans %in% c("y", "yes"))
}

# Shared body for the per-artifact resets. Returns TRUE on success,
# FALSE on cancellation, TRUE invisibly when the save dir is missing.
reset_save_artifact <- function(prompt, paths, force) {
  d <- save_dir()
  if (is.null(d) || !dir.exists(d)) return(invisible(TRUE))
  if (!confirm(prompt, force)) return(invisible(FALSE))
  for (p in paths) unlink(file.path(d, p), recursive = TRUE)
  invisible(TRUE)
}

#' @rdname reset
#' @export
reset_career <- function(force = FALSE) {
  reset_save_artifact(
    "Reset career? This deletes all saved meta and runs. [y/N]: ",
    c("meta.json", "runs", "recent_messages.rds"),
    force
  )
}

#' @rdname reset
#' @export
reset_achievements <- function(force = FALSE) {
  reset_save_artifact("Reset achievements? [y/N]: ", "achievements.rds", force)
}

#' @rdname reset
#' @export
reset_wardrobe <- function(force = FALSE) {
  reset_save_artifact("Reset wardrobe? [y/N]: ", "wardrobe.rds", force)
}

#' @rdname reset
#' @export
reset_all <- function(force = FALSE) {
  d <- save_dir()
  if (is.null(d) || !dir.exists(d)) return(invisible(TRUE))
  if (!confirm("Reset everything (career + achievements + wardrobe + sightings)? [y/N]: ",
               force)) {
    return(invisible(FALSE))
  }
  unlink(d, recursive = TRUE)
  invisible(TRUE)
}
