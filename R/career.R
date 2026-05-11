#' Inspect career state
#'
#' Returns a `tx_career` object describing the persistent career, or a
#' fresh default if no save exists. The object's `print` method shows
#' a compact dashboard card; `summary` adds aggregate counts. Career
#' tier is derived from the publication-chain length you've unlocked
#' (see [progress()]); XP comes from completed chain stages.
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
  prog   <- progression_of(meta)

  fav_method   <- favorite_method(runs)
  most_removed <- most_removed_var(runs)
  next_thr     <- next_xp_threshold(prog$length_unlocked)

  obj <- list(
    level                 = career_level_for_length(prog$length_unlocked),
    runs_count            = length(runs),
    favorite_method       = fav_method,
    most_removed_variable = most_removed,
    xp                    = prog$xp,
    xp_next_unlock        = next_thr,
    chain_length          = prog$length_unlocked,
    chains_completed      = prog$chains_completed,
    chains_broken         = prog$chains_broken,
    active_chain_hud      = format_chain_hud(meta),
    achievements_unlocked = sum(vapply(ach, isTRUE, logical(1))),
    achievements_total    = nrow(load_achievement_registry()),
    wardrobe              = wd,
    meta                  = meta,
    runs_summary          = runs
  )
  class(obj) <- "tx_career"
  obj
}

# XP cost to reach the next chain length, or NA if at the cap.
next_xp_threshold <- function(length_unlocked) {
  i <- as.integer(length_unlocked %||% 1L) + 1L
  if (i > length(CHAIN_XP_THRESHOLDS)) return(NA_integer_)
  CHAIN_XP_THRESHOLDS[[i]]
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
  cat(sprintf("Most removed variable: %s\n",
              x$most_removed_variable %||% "-"))
  cat(sprintf("Chain length:          %d / %d\n",
              x$chain_length, length(CHAIN_STAGES)))
  if (!is.na(x$xp_next_unlock)) {
    cat(sprintf("XP:                    %d / %d (next unlock)\n",
                x$xp, x$xp_next_unlock))
  } else {
    cat(sprintf("XP:                    %d (chain fully unlocked)\n",
                x$xp))
  }
  cat(sprintf("Chains completed:      %d\n", x$chains_completed))
  if (!is.null(x$active_chain_hud)) {
    cat("\n", x$active_chain_hud, "\n", sep = "")
  }
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
  cat(sprintf("Chains broken:         %d\n", object$chains_broken))
  invisible(object)
}

#' @export
#' @noRd
#' @method format tx_career
format.tx_career <- function(x, ...) {
  paste0(x$level, " - ", x$runs_count, " runs")
}

# Update meta with the latest run's contributions. Career tier no
# longer comes from this path — it tracks XP via the chain mechanic.
# What still lives here: hidden component bookkeeping (for
# achievements), per-run counters, and the favorite-method etc.
# derivations.
update_career <- function(meta, run) {
  hid <- meta$hidden %||% list()
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
  prog <- progression_of(meta)
  new_level <- career_level_for_length(prog$length_unlocked)
  meta$career_level <- new_level

  attr(meta, "promoted")   <- !identical(prev_level, new_level)
  attr(meta, "from_level") <- prev_level
  attr(meta, "to_level")   <- new_level
  meta
}
