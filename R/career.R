#' Inspect career state
#'
#' Returns a `tx_career` object describing the persistent career, or a
#' fresh default if no save exists. The object's `print` method shows
#' a deadpan dashboard card; `summary` adds aggregate counts.
#'
#' @return A `tx_career` object.
#' @export
career <- function() {
  meta <- read_meta()
  if (is.null(meta)) {
    meta <- default_meta(save_dir() %||% tempfile())
  }

  runs   <- recent_run_records()
  ach    <- read_achievements_state()
  wd     <- read_wardrobe_state()

  fav_method <- favorite_method(runs)
  most_removed <- most_removed_var(runs)

  hidden <- meta$hidden %||% default_meta("")$hidden
  obj <- list(
    level                 = meta$career_level %||% "Junior Researcher",
    runs_count            = length(runs),
    favorite_method       = fav_method,
    most_removed_variable = most_removed,
    reviewer_compat       = qualitative(hidden$reviewer_resistance %||% 0),
    presentation_aest     = qualitative(hidden$presentation_quality %||% 0),
    achievements_unlocked = sum(vapply(ach, isTRUE, logical(1))),
    achievements_total    = nrow(load_achievement_registry()),
    wardrobe              = wd,
    meta                  = meta,
    runs_summary          = runs
  )
  class(obj) <- "tx_career"
  obj
}

# Map a hidden numeric score to an opaque qualitative label.
qualitative <- function(x) {
  if (is.null(x) || !is.finite(x)) return("Adequate")
  if (x < -0.10) return("Low")
  if (x <  0.10) return("Adequate")
  if (x <  0.30) return("High")
  "Exceptional"
}

favorite_method <- function(runs) {
  if (length(runs) == 0L) return(NA_character_)
  fams <- unlist(lapply(runs, function(r) r$highlighted_spec$family %||% NA))
  fams <- fams[!is.na(fams)]
  if (length(fams) == 0L) return(NA_character_)
  names(sort(table(fams), decreasing = TRUE))[1]
}

most_removed_var <- function(runs) {
  if (length(runs) == 0L) return(NA_character_)
  removed <- unlist(lapply(runs, function(r) r$highlighted_spec$dropped %||% character()))
  if (length(removed) == 0L) return(NA_character_)
  names(sort(table(removed), decreasing = TRUE))[1]
}

recent_run_records <- function(n = 50L) {
  files <- list_run_records()
  if (length(files) == 0L) return(list())
  files <- files[order(file.info(files)$mtime, decreasing = TRUE)]
  files <- utils::head(files, n)
  lapply(files, readRDS)
}

#' @export
#' @noRd
#' @method print tx_career
print.tx_career <- function(x, ...) {
  rule <- strrep("-", 48)
  cat(style_header(rule), "\n", sep = "")
  cat(style_header(sprintf("Career: %s", x$level)), "\n", sep = "")
  cat(style_header(rule), "\n", sep = "")
  cat(sprintf("Runs completed:        %d\n", x$runs_count))
  cat(sprintf("Favorite method:       %s\n",
              x$favorite_method %||% "-"))
  cat(sprintf("Reviewer compatibility:%s %s\n", "",
              x$reviewer_compat))
  cat(sprintf("Most removed variable: %s\n",
              x$most_removed_variable %||% "-"))
  cat(sprintf("Presentation aesthetics:%s %s\n", "",
              x$presentation_aest))
  invisible(x)
}

#' @export
#' @noRd
#' @method summary tx_career
summary.tx_career <- function(object, ...) {
  print(object)
  cat("\n")
  cat(sprintf("Achievements unlocked: %d / %d\n",
              object$achievements_unlocked, object$achievements_total))
  cat(sprintf("Models fit (lifetime): %d\n",
              as.integer(object$meta$hidden$models_fit %||% 0)))
  cat(sprintf("Outputs generated:     %d\n",
              as.integer(object$meta$hidden$output_complexity %||% 0)))
  invisible(object)
}

#' @export
#' @noRd
#' @method format tx_career
format.tx_career <- function(x, ...) {
  paste0(x$level, " - ", x$runs_count, " runs")
}

# -- Hidden composite score ------------------------------------------
#
# career_score = models_fit
#              * output_complexity
#              * reviewer_resistance
#              * narrative_strength
#              * h_index_proxy
#              * presentation_quality
#
# Components live in meta$hidden. They're each soft-bounded and
# combined multiplicatively (with an additive offset to avoid zero
# annihilation). The score itself is never printed; only opaque
# qualitative feedback is surfaced.

career_score <- function(hidden) {
  comp <- function(x) 1 + (x %||% 0)
  comp(hidden$models_fit / 100) *
    comp(hidden$output_complexity / 10) *
    comp(hidden$reviewer_resistance) *
    comp(hidden$narrative_strength) *
    comp(hidden$h_index_proxy) *
    comp(hidden$presentation_quality)
}

# Tier thresholds. Tuned for: reach Postdoc after a handful of runs
# with at least one output; Senior after sustained activity; PI after
# heavy career-long investment.
CAREER_TIERS <- list(
  list(level = "Junior Researcher", min = 0),
  list(level = "Postdoc",           min = 3),
  list(level = "Senior Scientist",  min = 12),
  list(level = "PI",                min = 48)
)

career_tier_for <- function(score) {
  out <- CAREER_TIERS[[1]]$level
  for (t in CAREER_TIERS) if (score >= t$min) out <- t$level
  out
}

# Update meta with the latest run's contributions and possibly fire
# a promotion event. Called from shoot() at end-of-run.
update_career <- function(meta, run) {
  hid <- meta$hidden
  hid$models_fit          <- (hid$models_fit %||% 0) + (run$spec_count %||% 0)
  hid$reviewer_resistance <- (hid$reviewer_resistance %||% 0) +
    (run$modifiers$reviewer_resistance %||% 0) * 0.05
  hid$narrative_strength  <- (hid$narrative_strength %||% 0) +
    (run$modifiers$narrative_strength %||% 0) * 0.05
  hid$presentation_quality <- (hid$presentation_quality %||% 0) +
    (run$modifiers$presentation_pressure %||% 0) * 0.05
  meta$hidden <- hid
  meta$runs_count <- (meta$runs_count %||% 0L) + 1L

  prev_level <- meta$career_level
  new_level  <- career_tier_for(career_score(hid))
  meta$career_level <- new_level

  attr(meta, "promoted") <- !identical(prev_level, new_level)
  attr(meta, "from_level") <- prev_level
  attr(meta, "to_level")   <- new_level
  meta
}
