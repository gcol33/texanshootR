# Output-generator unlock model. Each generator maps to a chain stage
# (see R/chain.R). The legacy career-tier UNLOCK_REGISTRY remains here
# only as the metadata `progress()` displays — the actual gate is the
# XP-driven chain length, checked via require_chain_stage() from each
# generator's source file.

# Maps each gated function to the chain stage it produces. The chain
# stage index dictates the XP required (CHAIN_XP_THRESHOLDS).
GENERATOR_STAGE <- list(
  abstract           = "abstract",
  manuscript         = "manuscript",
  presentation       = "presentation",
  reviewer_response  = "reviewer_response",
  graphical_abstract = "graphical_abstract",
  funding            = "funding"
)

# Retained for legacy progress() rows and tests that reference tier
# labels. Derived from chain index: the lowest tier at which each
# stage's chain length becomes unlocked.
UNLOCK_REGISTRY <- list(
  manuscript         = "Postdoc",
  reviewer_response  = "Senior Scientist",
  graphical_abstract = "Senior Scientist",
  presentation       = "Senior Scientist",
  funding            = "PI"
)

tier_index <- function(level) {
  tiers <- unique(CHAIN_TIER_BY_LENGTH)
  m <- match(level, tiers)
  if (is.na(m)) return(0L)
  m
}

current_career_level <- function() {
  meta <- tryCatch(read_meta(), error = function(e) NULL)
  if (is.null(meta)) return("Junior Researcher")
  prog <- progression_of(meta)
  career_level_for_length(prog$length_unlocked)
}

is_unlocked <- function(fn_name, level = current_career_level()) {
  stage <- GENERATOR_STAGE[[fn_name]]
  if (is.null(stage)) return(TRUE)
  meta <- read_meta() %||% default_meta(save_dir() %||% tempfile())
  prog <- progression_of(meta)
  stage_index(stage) <= prog$length_unlocked
}

format_locked_message <- function(fn_name, required, current) {
  paste0(
    fn_name, "() requires:\n",
    required, "\n",
    "\n",
    "Current career:\n",
    current
  )
}

# Legacy-shaped check kept for backwards compatibility with anything
# that still calls it. New code routes through require_chain_stage()
# in R/chain.R. We re-implement on top of progression so the two views
# stay consistent.
require_unlocked <- function(fn_name) {
  if (is_unlocked(fn_name)) return(invisible(TRUE))
  required <- UNLOCK_REGISTRY[[fn_name]] %||% "Postdoc"
  current  <- current_career_level()
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

# Achievement progress extractors. Each takes (runs, meta) and returns
# list(have, need, label).
ACHIEVEMENT_PROGRESS <- list(
  ach_multiple_comparisons = function(runs, meta) {
    best <- max(c(0L, vapply(runs,
                              function(r) as.integer(r$spec_count %||% 0L),
                              integer(1))))
    list(have = best, need = 1000L, label = "best run")
  },
  ach_var_purge = function(runs, meta) {
    best <- max(c(0L, vapply(runs,
                              function(r) length(r$highlighted_spec$dropped %||% character()),
                              integer(1))))
    list(have = as.integer(best), need = 5L, label = "best run")
  },
  ach_publication_pipeline = function(runs, meta) {
    have <- as.integer(meta$hidden$output_complexity %||% 0)
    list(have = have, need = 3L, label = "career")
  },
  ach_event_survivor = function(runs, meta) {
    have <- length(meta$events_witnessed %||% list())
    list(have = as.integer(have), need = 3L, label = "witnessed")
  },
  ach_all_outputs = function(runs, meta) {
    best <- max(c(0L, vapply(runs,
                              function(r) length(r$outputs_generated %||% character()),
                              integer(1))))
    list(have = as.integer(best), need = 6L, label = "best run")
  }
)

inflight_progress_rows <- function(runs, meta, ach_state, ach_reg) {
  ids <- intersect(names(ACHIEVEMENT_PROGRESS), ach_reg$id)
  out <- list()
  for (id in ids) {
    if (isTRUE(ach_state[[id]])) next
    row <- ach_reg[ach_reg$id == id, ]
    if (!isTRUE(row$visible)) next
    p <- ACHIEVEMENT_PROGRESS[[id]](runs, meta)
    if (is.null(p)) next
    out[[length(out) + 1L]] <- list(
      id    = id,
      name  = row$name,
      have  = p$have,
      need  = p$need,
      label = p$label
    )
  }
  out
}

#' Inspect API unlock progress
#'
#' Prints a HUD-style summary of unlock state for gated functions and
#' achievements. Reads live save state.
#'
#' Three call modes:
#' \itemize{
#'   \item `progress()` -- overview: career tier, gated-function lock
#'     map, achievement / wardrobe counts, and in-flight progress.
#'   \item `progress("manuscript")` -- per-function card.
#'   \item `progress("ach_multiple_comparisons")` -- per-achievement
#'     card.
#' }
#'
#' @param what Optional. A gated-function name (e.g. `"manuscript"`) or
#'   an achievement id (e.g. `"ach_multiple_comparisons"`). When NULL,
#'   prints the overview.
#' @return A `tx_progress` object (invisible).
#' @export
progress <- function(what = NULL) {
  current <- current_career_level()
  if (is.null(what)) {
    obj <- progress_overview(current)
  } else if (!is.null(UNLOCK_REGISTRY[[what]]) ||
             !is.null(GENERATOR_STAGE[[what]])) {
    obj <- progress_function(what, current)
  } else if (startsWith(what, "ach_")) {
    obj <- progress_achievement(what)
  } else {
    stop(sprintf("`%s` is not a gated function or achievement id.", what),
         call. = FALSE)
  }
  print(obj)
  invisible(obj)
}

progress_overview <- function(current) {
  meta <- read_meta() %||% default_meta(save_dir() %||% tempfile())
  ach_state <- read_achievements_state()
  ach_reg   <- load_achievement_registry()
  cos_reg   <- load_cosmetic_registry()
  wd        <- read_wardrobe_state()
  runs      <- recent_run_records()
  prog      <- progression_of(meta)

  rows <- lapply(names(UNLOCK_REGISTRY), function(nm) {
    list(fn = nm, required = UNLOCK_REGISTRY[[nm]],
         unlocked = is_unlocked(nm, current))
  })

  ach_unlocked <- sum(vapply(ach_state, isTRUE, logical(1)))
  ach_total    <- nrow(ach_reg)
  cos_unlocked <- length(wd$unlocked %||% character())
  cos_total    <- nrow(cos_reg)

  inflight <- inflight_progress_rows(runs, meta, ach_state, ach_reg)

  structure(list(
    mode      = "overview",
    current   = current,
    rows      = rows,
    chain_length = prog$length_unlocked,
    xp        = prog$xp,
    xp_next   = next_xp_threshold(prog$length_unlocked),
    active_chain_hud = format_chain_hud(meta),
    ach_unlocked = ach_unlocked,
    ach_total    = ach_total,
    cos_unlocked = cos_unlocked,
    cos_total    = cos_total,
    inflight  = inflight
  ), class = "tx_progress")
}

progress_function <- function(fn, current) {
  required <- UNLOCK_REGISTRY[[fn]] %||% "Junior Researcher"
  structure(list(
    mode     = "function",
    fn       = fn,
    current  = current,
    required = required,
    unlocked = is_unlocked(fn, current)
  ), class = "tx_progress")
}

progress_achievement <- function(id) {
  reg <- load_achievement_registry()
  if (!(id %in% reg$id)) {
    stop(sprintf("`%s` is not a known achievement id.", id), call. = FALSE)
  }
  state <- read_achievements_state()
  row <- reg[reg$id == id, ]
  unlocked <- isTRUE(state[[id]])
  hidden_locked <- !unlocked && !isTRUE(row$visible)

  obj <- list(
    mode     = "achievement",
    id       = id,
    name     = if (hidden_locked) "???" else row$name,
    unlocked = unlocked,
    hidden_locked = hidden_locked,
    detail   = NULL
  )
  if (!hidden_locked) {
    extractor <- ACHIEVEMENT_PROGRESS[[id]]
    if (!unlocked && !is.null(extractor)) {
      meta <- read_meta() %||% default_meta(save_dir() %||% tempfile())
      runs <- recent_run_records()
      obj$detail <- extractor(runs, meta)
    }
  }
  structure(obj, class = "tx_progress")
}

#' @export
#' @noRd
#' @method print tx_progress
print.tx_progress <- function(x, ...) {
  switch(x$mode,
    overview    = print_progress_overview(x),
    "function"  = print_progress_function(x),
    achievement = print_progress_achievement(x))
  invisible(x)
}

print_progress_overview <- function(x) {
  rule <- strrep("-", 48)
  cat(style_header(rule), "\n", sep = "")
  cat(style_header(sprintf("Career: %s", x$current)), "\n", sep = "")
  cat(style_header(rule), "\n", sep = "")
  cat(sprintf("Chain length:  %d / %d\n", x$chain_length,
              length(CHAIN_STAGES)))
  if (!is.na(x$xp_next)) {
    cat(sprintf("XP:            %d / %d (next unlock)\n",
                x$xp, x$xp_next))
  } else {
    cat(sprintf("XP:            %d (chain fully unlocked)\n", x$xp))
  }
  cat("\n")
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
      cat(sprintf("  %-*s requires %s\n", width,
                   paste0(r$fn, "()"), r$required))
    }
  }
  if (!is.null(x$active_chain_hud)) {
    cat("\n", x$active_chain_hud, "\n", sep = "")
  }
  cat(sprintf("\nAchievements: %d / %d\n", x$ach_unlocked, x$ach_total))
  cat(sprintf("Wardrobe:     %d / %d\n", x$cos_unlocked, x$cos_total))
  if (length(x$inflight) > 0L) {
    cat("\nIn-flight:\n")
    name_w  <- max(nchar(vapply(x$inflight, `[[`, character(1), "name")))
    label_w <- max(nchar(vapply(x$inflight, `[[`, character(1), "label")))
    for (r in x$inflight) {
      cat(sprintf("  %-*s   %-*s %d / %d\n",
                   name_w, r$name,
                   label_w + 1L, paste0(r$label, ":"),
                   r$have, r$need))
    }
  }
}

print_progress_function <- function(x) {
  if (x$unlocked) {
    cat(x$fn, "()\n", sep = "")
    cat("status: unlocked\n")
  } else {
    cat(format_locked_message(x$fn, x$required, x$current), "\n", sep = "")
  }
}

print_progress_achievement <- function(x) {
  cat(x$id, ":\n", sep = "")
  cat(x$name, "\n", sep = "")
  cat("\nStatus:\n")
  if (x$unlocked) {
    cat("unlocked\n")
  } else if (x$hidden_locked) {
    cat("locked\n")
  } else {
    cat("locked\n")
    if (!is.null(x$detail)) {
      cat(sprintf("\n%s:\n%d / %d\n",
                   tools::toTitleCase(x$detail$label),
                   x$detail$have, x$detail$need))
    }
  }
}
