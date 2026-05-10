# Career-tier gating for the output-generator API. The four tiers in
# CAREER_TIERS (R/career.R) are the only gating axis. Each gated function
# calls `require_unlocked()` at the top; locked calls signal a `tx_locked`
# condition with a deadpan two-block status message.

UNLOCK_REGISTRY <- list(
  manuscript         = "Postdoc",
  preprint           = "Postdoc",
  reviewer_response  = "Senior Scientist",
  graphical_abstract = "Senior Scientist",
  presentation       = "Senior Scientist",
  funding            = "PI"
)

tier_index <- function(level) {
  for (i in seq_along(CAREER_TIERS)) {
    if (identical(CAREER_TIERS[[i]]$level, level)) return(i)
  }
  0L
}

current_career_level <- function() {
  meta <- tryCatch(read_meta(), error = function(e) NULL)
  meta$career_level %||% "Junior Researcher"
}

is_unlocked <- function(fn_name, level = current_career_level()) {
  required <- UNLOCK_REGISTRY[[fn_name]]
  if (is.null(required)) return(TRUE)
  tier_index(level) >= tier_index(required)
}

# Format the deadpan two-block status message. No second-person voice,
# no encouragement, no question marks - reads as a system message.
format_locked_message <- function(fn_name, required, current) {
  paste0(
    fn_name, "() requires:\n",
    required, "\n",
    "\n",
    "Current career:\n",
    current
  )
}

require_unlocked <- function(fn_name) {
  required <- UNLOCK_REGISTRY[[fn_name]]
  if (is.null(required)) return(invisible(TRUE))
  current <- current_career_level()
  if (tier_index(current) >= tier_index(required)) return(invisible(TRUE))
  cond <- structure(
    class = c("tx_locked", "error", "condition"),
    list(
      message  = format_locked_message(fn_name, required, current),
      call     = NULL,
      fn       = fn_name,
      required = required,
      current  = current
    )
  )
  stop(cond)
}

#' Inspect API unlock progress
#'
#' Prints a HUD-style summary of which gated functions are currently
#' unlocked, which are locked, and what the next unlock requires. Reads
#' live save state.
#'
#' Pairs with the static unlock requirement printed in each gated
#' function's help page (e.g. `?presentation`). The help page documents
#' the requirement; `progress()` reports the live state.
#'
#' @param fn Optional function name (string). If supplied, return the
#'   status for just that function.
#' @return A `tx_progress` object (invisible).
#' @export
progress <- function(fn = NULL) {
  current <- current_career_level()
  reg <- UNLOCK_REGISTRY
  if (!is.null(fn)) {
    if (is.null(reg[[fn]])) {
      stop(sprintf("`%s` is not a gated function.", fn), call. = FALSE)
    }
    reg <- reg[fn]
  }
  rows <- lapply(names(reg), function(nm) {
    list(fn = nm, required = reg[[nm]],
         unlocked = tier_index(current) >= tier_index(reg[[nm]]))
  })
  obj <- list(current = current, rows = rows)
  class(obj) <- "tx_progress"
  print(obj)
  invisible(obj)
}

#' @export
#' @noRd
#' @method print tx_progress
print.tx_progress <- function(x, ...) {
  rule <- strrep("-", 48)
  cat(style_header(rule), "\n", sep = "")
  cat(style_header(sprintf("Career: %s", x$current)), "\n", sep = "")
  cat(style_header(rule), "\n", sep = "")
  unlocked <- Filter(function(r) r$unlocked, x$rows)
  locked   <- Filter(function(r) !r$unlocked, x$rows)
  if (length(unlocked) > 0L) {
    cat("Unlocked:\n")
    for (r in unlocked) cat(sprintf("  %s()\n", r$fn))
  }
  if (length(locked) > 0L) {
    if (length(unlocked) > 0L) cat("\n")
    cat("Locked:\n")
    width <- max(nchar(vapply(locked, function(r) r$fn, character(1)))) + 2L
    for (r in locked) {
      cat(sprintf("  %-*s requires %s\n", width, paste0(r$fn, "()"), r$required))
    }
  }
  invisible(x)
}
