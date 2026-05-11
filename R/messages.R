#' Validate the on-disk message registry
#'
#' Lints every YAML file under `inst/messages/` (and an additional path
#' if supplied), enforcing required fields, vocabulary membership, id
#' uniqueness, and combo-chain integrity. Used by the test suite and
#' available to contributors.
#'
#' @param path Optional character path to a directory of YAML files.
#'   When `NULL`, validates the package's bundled messages.
#' @return A data.frame of the validated registry, returned invisibly.
#' @export
validate_messages <- function(path = NULL) {
  reg <- load_message_registry(path = path)
  invisible(reg)
}

# Load and validate. Cached on the package environment for repeat use.
load_message_registry <- function(path = NULL, force = FALSE) {
  if (!force && is.null(path) && !is.null(.tx$messages)) {
    return(.tx$messages)
  }

  dir <- path %||% system.file("messages", package = "texanshootR")
  if (!nzchar(dir) || !dir.exists(dir)) {
    # Empty registry is allowed during development.
    reg <- empty_message_registry()
    if (is.null(path)) .tx$messages <- reg
    return(reg)
  }

  files <- list.files(dir, pattern = "\\.ya?ml$", full.names = TRUE)
  rows  <- list()
  for (f in files) {
    entries <- yaml::read_yaml(f)
    if (is.null(entries)) next
    for (i in seq_along(entries)) {
      rows[[length(rows) + 1L]] <- normalise_message_entry(entries[[i]], f, i)
    }
  }
  if (length(rows) == 0L) {
    reg <- empty_message_registry()
  } else {
    reg <- rbind_messages(rows)
    validate_registry(reg)
  }
  if (is.null(path)) .tx$messages <- reg
  reg
}

empty_message_registry <- function() {
  data.frame(
    id                    = character(),
    text                  = character(),
    tags                  = I(list()),
    rarity                = character(),
    career_min            = character(),
    trigger_phase         = character(),
    model_family_affinity = I(list()),
    mascot_state_affinity = I(list()),
    combo_start           = character(),
    combo_next            = character(),
    requires              = I(list()),
    source_file           = character(),
    stringsAsFactors      = FALSE
  )
}

# Coerce one YAML entry into a normalised one-row record.
normalise_message_entry <- function(e, f, i) {
  required <- c("id", "text", "rarity", "trigger_phase")
  missing <- setdiff(required, names(e))
  if (length(missing)) {
    stop(sprintf(
      "Message in %s entry %d missing required fields: %s",
      basename(f), i, paste(missing, collapse = ", ")
    ), call. = FALSE)
  }
  list(
    id                    = as.character(e$id),
    text                  = as.character(e$text),
    tags                  = as.character(e$tags %||% character()),
    rarity                = as.character(e$rarity),
    career_min            = if (is.null(e$career_min) || is.na(e$career_min))
                              NA_character_ else as.character(e$career_min),
    trigger_phase         = as.character(e$trigger_phase),
    model_family_affinity = as.character(e$model_family_affinity %||% character()),
    mascot_state_affinity = as.character(e$mascot_state_affinity %||% character()),
    combo_start           = as.character(e$combo_chain$start %||% NA_character_),
    combo_next            = as.character(e$combo_chain[["next"]] %||% NA_character_),
    requires              = as.character(e$requires %||% character()),
    source_file           = basename(f)
  )
}

rbind_messages <- function(rows) {
  flat <- function(field) lapply(rows, `[[`, field)
  data.frame(
    id                    = vapply(rows, `[[`, "", "id"),
    text                  = vapply(rows, `[[`, "", "text"),
    tags                  = I(flat("tags")),
    rarity                = vapply(rows, `[[`, "", "rarity"),
    career_min            = vapply(rows, function(r) r$career_min %||% NA_character_, ""),
    trigger_phase         = vapply(rows, `[[`, "", "trigger_phase"),
    model_family_affinity = I(flat("model_family_affinity")),
    mascot_state_affinity = I(flat("mascot_state_affinity")),
    combo_start           = vapply(rows, `[[`, "", "combo_start"),
    combo_next            = vapply(rows, `[[`, "", "combo_next"),
    requires              = I(flat("requires")),
    source_file           = vapply(rows, `[[`, "", "source_file"),
    stringsAsFactors      = FALSE
  )
}

validate_registry <- function(reg) {
  # Unique ids.
  dups <- reg$id[duplicated(reg$id)]
  if (length(dups)) {
    stop("Duplicate message ids: ", paste(unique(dups), collapse = ", "), call. = FALSE)
  }

  # Vocabulary membership.
  bad_phase <- setdiff(reg$trigger_phase, vocab_phases)
  if (length(bad_phase)) {
    stop("Unknown trigger_phase value(s): ", paste(bad_phase, collapse = ", "),
         ". See `vocab_phases`.", call. = FALSE)
  }
  bad_rarity <- setdiff(reg$rarity, c("common", "uncommon", "rare", "legendary"))
  if (length(bad_rarity)) {
    stop("Unknown rarity value(s): ", paste(bad_rarity, collapse = ", "), call. = FALSE)
  }
  career <- reg$career_min[!is.na(reg$career_min)]
  bad_career <- setdiff(career, vocab_careers)
  if (length(bad_career)) {
    stop("Unknown career_min value(s): ", paste(bad_career, collapse = ", "), call. = FALSE)
  }
  all_tags <- unique(unlist(reg$tags))
  bad_tags <- setdiff(all_tags, vocab_tags)
  if (length(bad_tags)) {
    stop("Unknown tag(s): ", paste(bad_tags, collapse = ", "),
         ". See `vocab_tags`.", call. = FALSE)
  }
  all_states <- unique(unlist(reg$mascot_state_affinity))
  bad_states <- setdiff(all_states, vocab_mascot_states)
  if (length(bad_states)) {
    stop("Unknown mascot_state_affinity value(s): ",
         paste(bad_states, collapse = ", "), call. = FALSE)
  }

  # Phases that ARE the methodological mechanic must carry at least
  # one fallacy tag (per the content design contract). Phases that are
  # institutional ceremony (banner, promotion, reviewer, state
  # transition, daily) are exempt.
  # Phases that are institutional ceremony (banner, promotion, reviewer,
  # state transition, daily, chain transitions) are exempt from the
  # fallacy-tag requirement.
  fallacy_phases <- c("blip", "loading", "derived_escalation",
                       "ultra_rare", "event", "event_consequence")
  fallacy_tags <- vocab_tags[seq_len(23)]
  fallacy_rows <- reg$trigger_phase %in% fallacy_phases
  has_fallacy <- vapply(reg$tags, function(t) any(t %in% fallacy_tags), logical(1))
  bad <- fallacy_rows & !has_fallacy
  if (any(bad)) {
    stop("Messages without a fallacy tag: ",
         paste(reg$id[bad], collapse = ", "), call. = FALSE)
  }

  # Combo-chain integrity: combo_next must reference a known combo_start.
  starts <- unique(reg$combo_start[!is.na(reg$combo_start)])
  bad_combo <- reg$combo_next[!is.na(reg$combo_next) & !(reg$combo_next %in% starts)]
  if (length(bad_combo)) {
    stop("Orphan combo_chain.next reference(s): ",
         paste(unique(bad_combo), collapse = ", "), call. = FALSE)
  }

  # Loading-slot budget: messages routed through ui_loading() in the
  # dynamic single-line TUI share one slot with the mascot and bar.
  # Anything longer than DYN_LOADING_BUDGET gets ellipsised at the
  # 120-col target width. Phases that hit ui_loading directly --
  # "loading" and "state_transition" -- are linted here. Other phases
  # write to the blip stream in multi-zone ANSI mode and don't share
  # the slot, so they're exempt.
  loading_phases <- c("loading", "state_transition")
  loading_rows <- reg$trigger_phase %in% loading_phases
  over_budget <- loading_rows & nchar(reg$text) > DYN_LOADING_BUDGET
  if (any(over_budget)) {
    offenders <- sprintf("%s (%d)", reg$id[over_budget],
                          nchar(reg$text[over_budget]))
    stop("Message text exceeds DYN_LOADING_BUDGET = ",
         DYN_LOADING_BUDGET, " chars: ",
         paste(offenders, collapse = ", "), call. = FALSE)
  }

  invisible(TRUE)
}

`%||%` <- function(a, b) if (is.null(a) || (length(a) == 1 && is.na(a))) b else a
