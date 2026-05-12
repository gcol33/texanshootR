# Live-injectable modifier registry.
#
# Replaces the flat SUPPORTED_MODIFIERS vector + scalar MODIFIER_BUDGET_BONUS
# in shoot.R with structured records. The registry is the single source
# of truth for:
#   - which tokens the +command parser accepts mid-run
#   - how much time each modifier adds to the shoot() deadline
#   - what bias each modifier imposes on the search engine
#   - what the TUI shows in the MODIFIERS row
#
# Time bonuses are per-modifier and reflect the "cost" of the move:
# heavyweight families (glmm, sem, derived_metrics) buy more deadline
# because their per-fit cost is higher; perturbation biases (subgroup,
# outliers) buy less because they only redirect the queue.
#
# Bias shape (the `bias` slot of each entry):
#   list(family       = character) -> force select_family() toward this fitter
#   list(perturb_tag  = character) -> push this kind to the head of the queue
#   list(escalate     = TRUE)      -> switch the run into desperation mode
# Biases compose by FIELD-WISE MERGE into state$bias. Different fields
# stack (a +glmm + +subgroup gives both a family bias AND a perturb bias);
# same-kind re-injection replaces (later +gam after +glmm switches the
# family priority). The escalate flag is one-shot sticky -- once set it
# stays set until the post-search desperation phase consumes it.

LIVE_MODIFIERS <- list(
  derived_metrics = list(
    token       = "derived_metrics",
    aliases     = c("derived", "dm"),
    display     = "+derived_metrics",
    time_bonus  = 5L,
    bias        = list(escalate = TRUE),
    description = "Trigger desperation: manufacture composite responses."
  ),
  subgroup = list(
    token       = "subgroup",
    aliases     = c("sub", "sg"),
    display     = "+subgroup",
    time_bonus  = 2L,
    bias        = list(perturb_tag = "restriction"),
    description = "Bias toward analytic-sample restrictions (factor-level, Cook's D, complete cases)."
  ),
  outliers = list(
    token       = "outliers",
    aliases     = c("out"),
    display     = "+outliers",
    time_bonus  = 2L,
    bias        = list(perturb_tag = "restriction"),
    description = "Bias toward principled-sounding sample exclusions."
  ),
  gam = list(
    token       = "gam",
    aliases     = character(),
    display     = "+gam",
    time_bonus  = 4L,
    bias        = list(family = "gam"),
    description = "Force the family selector toward GAM picks."
  ),
  glm = list(
    token       = "glm",
    aliases     = character(),
    display     = "+glm",
    time_bonus  = 3L,
    bias        = list(family = "glm"),
    description = "Force the family selector toward GLM picks."
  ),
  wls = list(
    token       = "wls",
    aliases     = character(),
    display     = "+wls",
    time_bonus  = 3L,
    bias        = list(family = "wls"),
    description = "Force the family selector toward weighted-LS picks."
  ),
  cor = list(
    token       = "cor",
    aliases     = character(),
    display     = "+cor",
    time_bonus  = 2L,
    bias        = list(family = "cor"),
    description = "Force the family selector toward bivariate correlation."
  ),
  glmm = list(
    token       = "glmm",
    aliases     = character(),
    display     = "+glmm",
    time_bonus  = 5L,
    bias        = list(family = "glmm"),
    description = "Force the family selector toward GLMM picks."
  ),
  sem = list(
    token       = "sem",
    aliases     = character(),
    display     = "+sem",
    time_bonus  = 5L,
    bias        = list(family = "sem"),
    description = "Force the family selector toward mediation SEM picks."
  ),
  aggregate = list(
    token       = "aggregate",
    aliases     = c("agg"),
    display     = "+aggregate",
    time_bonus  = 3L,
    bias        = list(perturb_tag = "aggregate"),
    description = "Collapse records to group means before fit."
  ),
  compilation = list(
    token       = "compilation",
    aliases     = c("comp"),
    display     = "+compilation",
    time_bonus  = 4L,
    bias        = list(perturb_tag = "compilation"),
    description = "Treat a literature compilation as ground-truth response."
  ),
  gapfill = list(
    token       = "gapfill",
    aliases     = c("gap"),
    display     = "+gapfill",
    time_bonus  = 3L,
    bias        = list(perturb_tag = "validate_on_fit"),
    description = "Validate the gap-fill on the cases that aren't gaps."
  ),
  tier2 = list(
    token       = "tier2",
    aliases     = c("relax", "downweight"),
    display     = "+tier2",
    time_bonus  = 3L,
    bias        = list(perturb_tag = "threshold_relax"),
    description = "Add a downweighted second tier when the cutoff is empty."
  ),
  counter_argument = list(
    token       = "counter_argument",
    aliases     = c("counter", "preempt"),
    display     = "+counter_argument",
    time_bonus  = 2L,
    bias        = list(perturb_tag = "preempt"),
    description = "Address the reviewer's likely objections in the manuscript."
  ),
  pseudo_spatial = list(
    token       = "pseudo_spatial",
    aliases     = c("coords"),
    display     = "+pseudo_spatial",
    time_bonus  = 3L,
    bias        = list(perturb_tag = "pseudo_spatial"),
    description = "Add geographic coordinates as fixed effects and call it spatial."
  ),
  omit_control = list(
    token       = "omit_control",
    aliases     = c("omit"),
    display     = "+omit_control",
    time_bonus  = 2L,
    bias        = list(perturb_tag = "omit_control"),
    description = "Drop a covariate because you don't want to interpret it."
  ),
  knife_edge = list(
    token       = "knife_edge",
    aliases     = c("labeling"),
    display     = "+knife_edge",
    time_bonus  = 3L,
    bias        = list(perturb_tag = "knife_edge"),
    description = "Define groups by a knife-edge presence rule on a continuous variable."
  ),
  iid_rescue = list(
    token       = "iid_rescue",
    aliases     = c("iid"),
    display     = "+iid_rescue",
    time_bonus  = 3L,
    bias        = list(perturb_tag = "iid_rescue"),
    description = "Reassign an inconvenient fixed effect to an iid random effect."
  ),
  defer_fix = list(
    token       = "defer_fix",
    aliases     = c("defer", "next_step"),
    display     = "+defer_fix",
    time_bonus  = 2L,
    bias        = list(perturb_tag = "defer"),
    description = "Acknowledge a methodological correction; defer it to a future paper."
  ),
  coffee_break = list(
    token       = "coffee_break",
    aliases     = c("side_meeting", "offline"),
    display     = "+coffee_break",
    time_bonus  = 2L,
    bias        = list(perturb_tag = "offline"),
    description = "Move methodological disagreement off the written record."
  ),
  mechanism_rebuttal = list(
    token       = "mechanism_rebuttal",
    aliases     = c("intuition", "but_biologically"),
    display     = "+mechanism_rebuttal",
    time_bonus  = 2L,
    bias        = list(perturb_tag = "intuition"),
    description = "Dismiss a statistical concern with domain-mechanism intuition."
  ),
  misunderstanding = list(
    token       = "misunderstanding",
    aliases     = c("you_misread"),
    display     = "+misunderstanding",
    time_bonus  = 2L,
    bias        = list(perturb_tag = "deflect_misread"),
    description = "Relocate the methodological problem from the analysis to the reader."
  ),
  narrow_answer = list(
    token       = "narrow_answer",
    aliases     = c("strawman"),
    display     = "+narrow_answer",
    time_bonus  = 2L,
    bias        = list(perturb_tag = "deflect_narrow"),
    description = "Answer a weaker version of the concern at the wrong level."
  ),
  complexity_shield = list(
    token       = "complexity_shield",
    aliases     = c("its_complex"),
    display     = "+complexity_shield",
    time_bonus  = 2L,
    bias        = list(perturb_tag = "deflect_complexity"),
    description = "Invoke unspecified complexity to dismiss a specific concern."
  ),
  frame_shift = list(
    token       = "frame_shift",
    aliases     = c("oscillate"),
    display     = "+frame_shift",
    time_bonus  = 2L,
    bias        = list(perturb_tag = "deflect_frame"),
    description = "Oscillate between descriptive and causal framing without flagging."
  ),
  matter_of_taste = list(
    token       = "matter_of_taste",
    aliases     = c("just_one_way"),
    display     = "+matter_of_taste",
    time_bonus  = 2L,
    bias        = list(perturb_tag = "deflect_taste"),
    description = "Soften a falsifiable concern into a matter of perspective."
  )
)

# ---- Parser ---------------------------------------------------------

# Resolve a raw input buffer (whatever the user typed at the tactical
# prompt) to a canonical modifier token, or NA_character_ when the input
# is empty / unknown.
#
# Accepts all of these for the same modifier:
#   "+glmm" "+ glmm" "glmm" "+glmm()" "+ GLMM " "  glmm  "
# Aliases declared in LIVE_MODIFIERS$<token>$aliases also resolve.
parse_mod_command <- function(buf) {
  if (is.null(buf)) return(NA_character_)
  s <- trimws(as.character(buf)[1])
  if (!nzchar(s)) return(NA_character_)
  # Strip a leading "+" with optional whitespace.
  s <- sub("^\\+\\s*", "", s)
  # Strip parens at the end ("glmm()" -> "glmm").
  s <- sub("\\s*\\(.*\\)\\s*$", "", s)
  s <- tolower(trimws(s))
  if (!nzchar(s)) return(NA_character_)

  for (token in names(LIVE_MODIFIERS)) {
    m <- LIVE_MODIFIERS[[token]]
    if (s == token || s %in% m$aliases) return(token)
  }
  NA_character_
}

# ---- Availability ---------------------------------------------------

# Tokens the player can still inject this run: not in `consumed`, and
# within career gates. Family modifiers inherit FAMILIES[[name]]$min_career;
# non-family modifiers (subgroup, outliers, derived_metrics) are
# tier-agnostic.
available_modifiers <- function(consumed = character(),
                                career_level = "Junior Researcher") {
  out <- character()
  for (token in names(LIVE_MODIFIERS)) {
    if (token %in% consumed) next
    if (!modifier_available_for_career(token, career_level)) next
    out <- c(out, token)
  }
  out
}

modifier_available_for_career <- function(token, career_level) {
  reg <- FAMILIES[[token]]
  if (is.null(reg)) return(TRUE)  # non-family modifier — no gate
  tier_index(career_level) >= tier_index(reg$min_career)
}

# Pre-roll a per-transition loadout for non-interactive runs (RStudio
# Console / dynamic mode). Returns a list keyed by mascot state name;
# values are either a token string or NA (skip this transition). The
# shoot() loop applies the pre-rolled token at each mascot transition
# instead of opening a tactical_pause readline that RStudio cannot
# render in place.
pre_roll_modifiers <- function(career_level) {
  states <- c("uncertain", "worried", "anxious", "panicked", "desperate")
  out    <- as.list(setNames(rep(NA_character_, length(states)), states))
  available <- available_modifiers(consumed = character(),
                                   career_level = career_level)
  if (!length(available)) return(out)
  n_picks <- sample.int(min(3L, length(available)), 1L)
  picks   <- sample(available, n_picks)
  slots   <- sample(states, n_picks)
  for (i in seq_along(picks)) out[[slots[i]]] <- picks[i]
  out
}

# Human-friendly list of the available modifiers for the MODIFIERS TUI
# row, e.g. "+gam  +derived  +subgroup".
format_modifier_row <- function(tokens) {
  if (!length(tokens)) return("(none)")
  paste(vapply(tokens, function(t) LIVE_MODIFIERS[[t]]$display, character(1)),
        collapse = "  ")
}

# ---- Apply ----------------------------------------------------------

# Default bias shape -- every state$bias holds these keys whether or not
# anything was injected. NA / FALSE are the "no override" sentinels the
# search engine treats as "use normal selection."
empty_bias <- function() {
  list(family      = NA_character_,
       perturb_tag = NA_character_,
       escalate    = FALSE)
}

# Apply a parsed modifier token to the run state. Returns the updated
# state. The caller renders the TUI afterwards. `time_bonus_callback` is
# an optional function that receives the seconds delta -- the loop uses
# it to extend `end_time` directly since end_time is held outside state.
apply_modifier <- function(state, token, time_bonus_callback = NULL) {
  m <- LIVE_MODIFIERS[[token]]
  if (is.null(m)) return(state)

  state$consumed_modifiers <- unique(c(state$consumed_modifiers %||% character(),
                                       token))
  if (is.null(state$bias)) state$bias <- empty_bias()

  # Field-wise merge: same-kind injection replaces, different-kind stacks.
  if (!is.null(m$bias$family))      state$bias$family      <- m$bias$family
  if (!is.null(m$bias$perturb_tag)) state$bias$perturb_tag <- m$bias$perturb_tag
  if (isTRUE(m$bias$escalate))       state$bias$escalate    <- TRUE

  if (is.function(time_bonus_callback)) {
    time_bonus_callback(m$time_bonus)
  }

  state$last_modifier <- list(token = token,
                              time_bonus = m$time_bonus,
                              bias = m$bias)
  state
}

# ---- Tactical pause -------------------------------------------------
#
# The user-facing readline interrupt. The shoot() loop calls this at
# escalation moments (mascot state transitions); the heartbeat freezes
# while the prompt is open, the user either types a +command or just
# hits Enter to skip, and the loop resumes.
#
# Returns list(state, end_time, applied). `applied` is the token that
# landed (NA_character_ when the prompt was skipped or rejected). The
# caller is responsible for updating its own end_time -- we return the
# new value so the caller can rebind it without state-list mutation.
#
# `prompt_fn` is injectable for testing (default: base::readline). In
# non-interactive sessions tactical_pause is a no-op.

tactical_pause <- function(state, end_time, ui,
                            career_level = "Junior Researcher",
                            prompt_fn = NULL) {
  if (!interactive() && is.null(prompt_fn)) {
    return(list(state = state, end_time = end_time, applied = NA_character_))
  }
  if (is.null(prompt_fn)) prompt_fn <- readline

  consumed  <- state$consumed_modifiers %||% character()
  available <- available_modifiers(consumed = consumed,
                                    career_level = career_level)
  if (!length(available)) {
    return(list(state = state, end_time = end_time, applied = NA_character_))
  }

  # Render via the TUI session when one is open; otherwise fall back to
  # plain stdout (covers non-interactive test runs with a mocked prompt_fn).
  emit <- function(text) {
    if (!is.null(ui)) ui_loading(ui, text) else cat(text, "\n", sep = "")
  }

  # The MODIFIERS row in the TUI already shows the live-available
  # tokens; the prompt itself is just the cue + input. Empty input skips.
  emit("METHODOLOGICAL PRIORITY WINDOW")

  buf   <- prompt_fn("+ ")
  token <- parse_mod_command(buf)

  if (is.na(token)) {
    emit("Submission abandoned.")
    return(list(state = state, end_time = end_time, applied = NA_character_))
  }
  if (!(token %in% available)) {
    emit(sprintf("+%s unavailable (consumed or career-locked).", token))
    return(list(state = state, end_time = end_time, applied = NA_character_))
  }

  new_end_time <- end_time
  state <- apply_modifier(state, token,
                          time_bonus_callback = function(secs) {
                            new_end_time <<- new_end_time + secs
                          })

  emit(sprintf("+%s applied. %s Deadline +%ds.",
               token,
               format_bias_announcement(state$last_modifier$bias),
               state$last_modifier$time_bonus))

  list(state = state, end_time = new_end_time, applied = token)
}

# One-line description of the bias a modifier imposes, used in the
# announcement that follows a successful injection.
format_bias_announcement <- function(bias) {
  if (isTRUE(bias$escalate)) return("Desperation escalation engaged.")
  if (!is.null(bias$family))
    return(sprintf("%s priority added.", toupper(bias$family)))
  if (!is.null(bias$perturb_tag))
    return(sprintf("Perturbation bias toward %s.", bias$perturb_tag))
  ""
}

# ---- Bias resolution for the search loop ---------------------------

# When a family bias is active, bypass select_family() and return the
# spec$family record directly. Mirrors the per-family pickers in
# family_select.R, but with the family chosen instead of sampled.
force_family <- function(family_token, outcome_vec) {
  outcome_kind <- classify_outcome(outcome_vec)
  switch(family_token,
    glm  = pick_glm_family(outcome_kind, flail = FALSE),
    gam  = pick_gam_family(outcome_kind),
    glmm = pick_glmm_family(outcome_kind),
    sem  = pick_sem_family(),
    cor  = pick_cor_family(),
    wls  = pick_wls_family(),
    list(fitter = "lm")
  )
}

# Single entry point for "what family should this spec be fit with?".
# Honors the active family bias when one is set and the requested family
# is available at the player's career tier; otherwise delegates to the
# regular weighted random selector.
pick_spec_family <- function(state, career_level, outcome_vec, family_pool,
                              escalating = FALSE) {
  fam <- state$bias$family %||% NA_character_
  if (!is.na(fam) && fam %in% family_pool) {
    return(force_family(fam, outcome_vec))
  }
  select_family(career_level, escalating = escalating,
                outcome = outcome_vec, available = family_pool)
}
