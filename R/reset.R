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

#' @rdname reset
#' @export
reset_career <- function(force = FALSE) {
  d <- save_dir()
  if (is.null(d) || !dir.exists(d)) return(invisible(TRUE))
  if (!confirm("Reset career? This deletes all saved meta and runs. [y/N]: ", force)) {
    return(invisible(FALSE))
  }
  unlink(file.path(d, "meta.json"))
  unlink(file.path(d, "runs"), recursive = TRUE)
  unlink(file.path(d, "recent_messages.rds"))
  invisible(TRUE)
}

#' @rdname reset
#' @export
reset_achievements <- function(force = FALSE) {
  d <- save_dir()
  if (is.null(d) || !dir.exists(d)) return(invisible(TRUE))
  if (!confirm("Reset achievements? [y/N]: ", force)) return(invisible(FALSE))
  unlink(file.path(d, "achievements.rds"))
  invisible(TRUE)
}

#' @rdname reset
#' @export
reset_wardrobe <- function(force = FALSE) {
  d <- save_dir()
  if (is.null(d) || !dir.exists(d)) return(invisible(TRUE))
  if (!confirm("Reset wardrobe? [y/N]: ", force)) return(invisible(FALSE))
  unlink(file.path(d, "wardrobe.rds"))
  invisible(TRUE)
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
