#' Inspect the achievement registry
#'
#' Returns a data.frame of all achievements with unlock status. Hidden
#' achievements appear with `name = "???"` and `hint = NA` until
#' unlocked.
#'
#' @return A data.frame.
#' @export
achievements <- function() {
  reg <- load_achievement_registry()
  state <- read_achievements_state()
  unlocked <- vapply(reg$id, function(id) isTRUE(state[[id]]), logical(1))
  display_name <- ifelse(unlocked | reg$visible, reg$name, "???")
  display_hint <- ifelse(unlocked | reg$visible, reg$hint, NA_character_)
  out <- data.frame(
    id         = reg$id,
    name       = display_name,
    visible    = reg$visible,
    unlocked   = unlocked,
    hint       = display_hint,
    stringsAsFactors = FALSE
  )
  out
}

# Lazy-loaded achievement registry. Combines all YAMLs under
# inst/achievements/.
load_achievement_registry <- function(force = FALSE) {
  if (!force && !is.null(.tx$achievements)) return(.tx$achievements)
  dir <- system.file("achievements", package = "texanshootR")
  if (!nzchar(dir) || !dir.exists(dir)) {
    .tx$achievements <- empty_achievement_registry()
    return(.tx$achievements)
  }
  files <- list.files(dir, pattern = "\\.ya?ml$", full.names = TRUE)
  rows <- list()
  for (f in files) {
    entries <- yaml::read_yaml(f)
    if (is.null(entries)) next
    for (e in entries) {
      rows[[length(rows) + 1L]] <- list(
        id           = as.character(e$id),
        name         = as.character(e$name),
        visible      = isTRUE(e$visible),
        hint         = if (is.null(e$hint) || is.na(e$hint)) NA_character_
                       else as.character(e$hint),
        reveal_text  = as.character(e$reveal_text %||% e$name),
        trigger      = as.character(e$trigger %||% NA_character_),
        cosmetic     = as.character(e$cosmetic %||% NA_character_)
      )
    }
  }
  if (length(rows) == 0L) {
    out <- empty_achievement_registry()
  } else {
    out <- data.frame(
      id          = vapply(rows, `[[`, "", "id"),
      name        = vapply(rows, `[[`, "", "name"),
      visible     = vapply(rows, `[[`, FALSE, "visible"),
      hint        = vapply(rows, `[[`, NA_character_, "hint"),
      reveal_text = vapply(rows, `[[`, "", "reveal_text"),
      trigger     = vapply(rows, `[[`, NA_character_, "trigger"),
      cosmetic    = vapply(rows, `[[`, NA_character_, "cosmetic"),
      stringsAsFactors = FALSE
    )
  }
  .tx$achievements <- out
  out
}

empty_achievement_registry <- function() {
  data.frame(
    id = character(), name = character(), visible = logical(),
    hint = character(), reveal_text = character(), trigger = character(),
    cosmetic = character(), stringsAsFactors = FALSE
  )
}

# Evaluate every trigger against a run + career snapshot. Returns a
# character vector of newly-awarded ids.
evaluate_achievements <- function(run, career_meta) {
  reg <- load_achievement_registry()
  state <- read_achievements_state()
  newly <- character()
  for (i in seq_len(nrow(reg))) {
    id <- reg$id[i]
    if (isTRUE(state[[id]])) next
    fn <- get0(reg$trigger[i], inherits = TRUE)
    if (is.null(fn)) next
    fired <- tryCatch(isTRUE(fn(run, career_meta)),
                      error = function(e) FALSE)
    if (fired) {
      state[[id]] <- TRUE
      newly <- c(newly, id)
    }
  }
  if (length(newly)) write_achievements_state(state)
  newly
}

# Run the evaluator and auto-equip cosmetics for any newly-awarded
# achievements. Returns the awarded ids.
award_and_equip <- function(run, career_meta) {
  awarded <- evaluate_achievements(run, career_meta)
  if (length(awarded)) {
    reg <- load_achievement_registry()
    for (id in awarded) {
      cid <- reg$cosmetic[reg$id == id]
      if (length(cid) && !is.na(cid) && nzchar(cid)) auto_equip(cid)
    }
  }
  awarded
}

# -- Trigger functions ------------------------------------------------
#
# Each function takes (run, career_meta) and returns TRUE on unlock.
# These are the v1 priority subset; expand as new achievements ship.

trigger_first_run <- function(run, meta) {
  (meta$runs_count %||% 0L) >= 1L
}

trigger_aim_after_data <- function(run, meta) {
  # "Aimed after seeing the data" - Texas sharpshooter. Triggers on
  # any successful run with >= 1 highlighted spec.
  !is.null(run$highlighted_spec)
}

trigger_p_hacked <- function(run, meta) {
  # Highlighted p in the just-cleared [0.040, 0.0499] strip.
  p <- run$highlighted_spec$p_value %||% NA_real_
  is.finite(p) && p >= 0.040 && p < 0.050
}

trigger_harker <- function(run, meta) {
  # HARKing: a manuscript was generated for this run with no a priori
  # hypothesis recorded. Run flags `harked` when manuscript() is called.
  isTRUE(run$harked)
}

trigger_multiple_comparisons <- function(run, meta) {
  (run$spec_count %||% 0L) >= 1000L
}

trigger_subgroup_fisher <- function(run, meta) {
  (run$search$subgroup_count %||% 0L) >= 1L &&
    !is.null(run$highlighted_spec$subgroup)
}

trigger_var_purge <- function(run, meta) {
  length(run$highlighted_spec$dropped %||% character()) >= 5L
}

trigger_outlier_excluder <- function(run, meta) {
  isTRUE(run$highlighted_spec$outliers_dropped > 0L)
}

trigger_derived_insight <- function(run, meta) {
  # Hidden achievement: first time the derived-metric escalation
  # produces a p <= 0.05 highlight.
  isTRUE(run$derived_used) &&
    is.finite(run$highlighted_spec$p_value %||% NA_real_) &&
    run$highlighted_spec$p_value <= 0.05
}

trigger_overfitter <- function(run, meta) {
  isTRUE((run$highlighted_spec$r_squared %||% 0) >= 0.99) &&
    (run$highlighted_spec$n_terms %||% 0L) >= 6L
}

trigger_publication_pipeline <- function(run, meta) {
  # Publication bias proxy: career has produced >= 3 outputs.
  (meta$hidden$output_complexity %||% 0) >= 3
}

trigger_late_resolver <- function(run, meta) {
  isTRUE(run$resolved_at_progress >= 0.90)
}

trigger_event_survivor <- function(run, meta) {
  length(meta$events_witnessed %||% list()) >= 3L
}

trigger_reviewer_repeller <- function(run, meta) {
  identical(run$reviewer_outcome, "reject")
}

trigger_glimpse <- function(run, meta) {
  isTRUE(run$ultra_rare_seen)
}

trigger_optional_stopping <- function(run, meta) {
  isTRUE(run$stopped_early)
}

trigger_all_outputs <- function(run, meta) {
  required <- c("manuscript", "reviewer_response",
                "graphical_abstract", "funding", "presentation")
  outs <- run$outputs_generated %||% character()
  all(required %in% outs)
}

trigger_filename_archaeologist <- function(run, meta) {
  any(grepl("figure_final_v2_REAL", run$outputs_generated_files %||% character(),
            fixed = TRUE))
}

trigger_visible_panic <- function(run, meta) {
  # Peak emotional state of "panicked" or worse at any point in the run.
  sev <- mascot_severity(run$peak_mascot %||% "composed")
  sev >= mascot_severity("panicked")
}

trigger_begging_the_wall <- function(run, meta) {
  # Peak hit "desperate" — either ran out the clock or escalated.
  sev <- mascot_severity(run$peak_mascot %||% "composed")
  sev >= mascot_severity("desperate")
}
