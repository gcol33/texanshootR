#' Controlled vocabularies for the message and event registries
#'
#' These exported character vectors define the valid values for the
#' `tags`, `career_min`, `trigger_phase`, `mascot_state_affinity`, and
#' event `effects` fields used in `inst/messages/*.yaml` and
#' `inst/events/*.yaml`. [validate_messages()] enforces them.
#'
#' @name vocab
NULL

#' @describeIn vocab Valid `tags` values. The lower-case fallacy
#'   names from the design contract plus thematic content tags.
#' @export
vocab_tags <- c(
  # Fallacy/sin tags (every message must carry at least one)
  "texas_sharpshooter",
  "p_hacking",
  "harking",
  "multiple_comparisons",
  "optional_stopping",
  "data_dredging",
  "cherry_picking",
  "outlier_exclusion",
  "subgroup_fishing",
  "target_leakage",
  "researcher_dof",
  "overfitting",
  "stepwise_abuse",
  "collider_bias",
  "omitted_variable",
  "post_hoc",
  "survivorship",
  "publication_bias",
  "salami_slicing",
  "story_first",
  "model_shopping",
  "causal_overreach",
  "validation_leakage",
  # Thematic tags
  "ecology",
  "ml",
  "phylo",
  "spatial",
  "temporal",
  "bayesian",
  "frequentist",
  "admin",
  "reviewer",
  "funding",
  "presentation",
  "narrative",
  "career",
  "onboarding"
)

#' @describeIn vocab Valid `trigger_phase` values.
#' @export
vocab_phases <- c(
  "blip",
  "loading",
  "promotion",
  "promotion_committee",
  "reviewer",
  "derived_escalation",
  "state_transition",
  "ultra_rare",
  "daily",
  "banner",
  "event",
  "event_consequence",
  "chain_opened",
  "stage_advanced",
  "chain_completed",
  "chain_broken"
)

#' @describeIn vocab Valid `career_min` values.
#' @export
vocab_careers <- c(
  "Junior Researcher",
  "Postdoc",
  "Senior Scientist",
  "PI"
)

#' @describeIn vocab Valid `mascot_state_affinity` values.
#' @export
vocab_mascot_states <- c(
  "composed",
  "uncertain",
  "anxious",
  "desperate",
  "resolved",
  "polishing",
  "submitting",
  "granted",
  "rejected"
)

#' @describeIn vocab Valid event-effect keys for `inst/events/*.yaml`.
#' @export
vocab_effects <- c(
  "throughput",
  "search_budget",
  "reviewer_resistance",
  "narrative_strength",
  "interpretation_confidence",
  "presentation_pressure",
  "funding_uncertainty",
  "typo_probability",
  "instability"
)

#' @describeIn vocab Career level ordering (low to high).
#' @keywords internal
career_levels <- function() {
  vocab_careers
}

#' Compare two career levels.
#'
#' @param a,b Career level strings.
#' @return Integer: -1 if a < b, 0 if equal, 1 if a > b.
#' @keywords internal
career_compare <- function(a, b) {
  ai <- match(a, vocab_careers)
  bi <- match(b, vocab_careers)
  if (is.na(ai) || is.na(bi)) {
    stop("Unknown career level: ", paste(c(a, b)[is.na(c(ai, bi))], collapse = ", "))
  }
  sign(ai - bi)
}
