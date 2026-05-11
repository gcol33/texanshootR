# Life-event encounters: random EVENT: blocks fired with probability
# 1/6 (configurable) per run. Each event applies temporary modifiers
# to the live run state.

load_event_registry <- function(force = FALSE) {
  load_registry(
    inst_dir   = "events",
    cache_key  = "events",
    entry_to_row = function(e) list(
      id               = as.character(e$id),
      event_text       = as.character(e$event_text),
      consequence_text = as.character(e$consequence_text),
      rarity           = as.character(e$rarity),
      career_min       = if (is.null(e$career_min) || is.na(e$career_min))
                           NA_character_ else as.character(e$career_min),
      display_phase    = as.character(e$display_phase %||% "any"),
      effects          = e$effects %||% list()
    ),
    rows_to_frame = function(rows) {
      out <- data.frame(
        id               = vapply(rows, `[[`, "", "id"),
        event_text       = vapply(rows, `[[`, "", "event_text"),
        consequence_text = vapply(rows, `[[`, "", "consequence_text"),
        rarity           = vapply(rows, `[[`, "", "rarity"),
        career_min       = vapply(rows, function(r) r$career_min %||% NA_character_, ""),
        display_phase    = vapply(rows, `[[`, "", "display_phase"),
        stringsAsFactors = FALSE
      )
      out$effects <- I(lapply(rows, `[[`, "effects"))
      out
    },
    empty = empty_event_registry,
    force = force
  )
}

empty_event_registry <- function() {
  new_list_col_frame(
    char_cols = c("id", "event_text", "consequence_text",
                  "rarity", "career_min", "display_phase"),
    list_col  = "effects"
  )
}

# Roll once per run. Returns NULL when nothing fires; otherwise an
# event record with applied effects.
maybe_event <- function(career_level, run_phase) {
  if (!isTRUE(opt("texanshootR.life_events"))) return(NULL)
  rate <- as.numeric(opt("texanshootR.event_rate"))
  if (!is.finite(rate) || rate <= 0) return(NULL)
  if (runif(1) > rate) return(NULL)

  reg <- load_event_registry()
  if (nrow(reg) == 0L) return(NULL)

  ok <- (reg$display_phase == "any") | (reg$display_phase == run_phase)
  ok <- ok & vapply(reg$career_min, function(c) {
    is.na(c) || career_compare(career_level, c) >= 0
  }, logical(1))
  if (!any(ok)) return(NULL)
  cand <- reg[ok, , drop = FALSE]
  w <- rarity_weight(cand$rarity)
  if (sum(w) == 0) return(NULL)
  idx <- sample.int(nrow(cand), size = 1L, prob = w)

  list(
    id               = cand$id[idx],
    event_text       = cand$event_text[idx],
    consequence_text = cand$consequence_text[idx],
    effects          = cand$effects[[idx]] %||% list()
  )
}

# Apply a single event's effects to a run-state list. `state` is the
# mutable accumulator threaded through shoot()'s loop.
apply_event_effects <- function(state, effects) {
  if (length(effects) == 0L) return(state)
  for (k in names(effects)) {
    if (!(k %in% vocab_effects)) next
    v <- effects[[k]]
    if (!is.numeric(v)) next
    v <- max(-0.5, min(0.5, v))
    state$modifiers[[k]] <- (state$modifiers[[k]] %||% 0) + v
  }
  state
}
