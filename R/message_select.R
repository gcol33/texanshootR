# Message selection algorithm.
#
# 1. Filter the registry by trigger_phase, career_min, requested tags,
#    model_family_affinity, mascot_state_affinity, and installed
#    package availability (entries with a `requires` list are dropped
#    if any required package is unavailable).
# 2. Assign rarity weights (common=1000, uncommon=100, rare=10,
#    legendary=1).
# 3. Apply recency suppression using the recent-id ring buffer.
#    Combo-chain follow-ups are exempt.
# 4. Apply combo-chain bias: if `combo_state` names a chain start,
#    boost weights for messages whose `combo_next` matches.
# 5. Weighted-sample one entry, return its id+text.

select_message <- function(phase,
                           career = NULL,
                           tags = NULL,
                           model_family = NULL,
                           mascot_state = NULL,
                           recent = character(),
                           combo_state = NULL,
                           tag_boost = NULL,
                           registry = NULL) {
  reg <- registry %||% load_message_registry()
  if (nrow(reg) == 0L) return(NULL)

  # 1. Filter.
  ok <- reg$trigger_phase == phase

  if (!is.null(career)) {
    ok <- ok & vapply(reg$career_min, function(c) {
      is.na(c) || career_compare(career, c) >= 0
    }, logical(1))
  }

  if (!is.null(tags) && length(tags)) {
    ok <- ok & vapply(reg$tags, function(t) any(t %in% tags), logical(1))
  }

  if (!is.null(model_family) && length(model_family)) {
    # If a message specifies affinities, at least one must match.
    # Messages with empty affinity are universal.
    ok <- ok & vapply(reg$model_family_affinity, function(a) {
      length(a) == 0L || any(a %in% model_family)
    }, logical(1))
  }

  if (!is.null(mascot_state)) {
    ok <- ok & vapply(reg$mascot_state_affinity, function(a) {
      length(a) == 0L || any(a %in% mascot_state)
    }, logical(1))
  }

  ok <- ok & vapply(reg$requires, function(req) {
    if (length(req) == 0L) return(TRUE)
    all(vapply(req, function(p) requireNamespace(p, quietly = TRUE), logical(1)))
  }, logical(1))

  if (!any(ok)) return(NULL)
  candidates <- reg[ok, , drop = FALSE]

  # 2. Rarity weights.
  w <- rarity_weight(candidates$rarity)

  # 3. Recency suppression: zero-out recent ids unless they're a combo
  #    follow-up to combo_state.
  if (length(recent)) {
    is_recent <- candidates$id %in% recent
    if (!is.null(combo_state)) {
      exempt <- candidates$combo_next == combo_state
      exempt[is.na(exempt)] <- FALSE
      is_recent <- is_recent & !exempt
    }
    w[is_recent] <- 0
  }

  # 4. Combo-chain bias: 10x weight for valid follow-ups.
  if (!is.null(combo_state)) {
    boost <- !is.na(candidates$combo_next) & candidates$combo_next == combo_state
    w[boost] <- w[boost] * 10
  }

  # 4b. Daily-flavor tag boost: 3x weight for entries carrying any of
  #     the boosted tags (e.g. Tabulation Tuesday boosts `p_hacking`).
  if (!is.null(tag_boost) && length(tag_boost)) {
    has_boost <- vapply(candidates$tags,
                        function(t) any(t %in% tag_boost), logical(1))
    w[has_boost] <- w[has_boost] * 3
  }

  if (sum(w) == 0) return(NULL)

  # 5. Weighted sample.
  idx <- sample.int(nrow(candidates), size = 1L, prob = w)
  list(
    id          = candidates$id[idx],
    text        = candidates$text[idx],
    combo_start = candidates$combo_start[idx]
  )
}

rarity_weight <- function(r) {
  out <- numeric(length(r))
  out[r == "common"]    <- 1000
  out[r == "uncommon"]  <- 100
  out[r == "rare"]      <- 10
  out[r == "legendary"] <- 1
  out
}

# Small ring-buffer helper for recent-message suppression.
recent_buffer <- function(size = 50L) {
  list(buf = character(size), head = 0L, n = 0L, size = size)
}

recent_push <- function(rb, id) {
  rb$head <- (rb$head %% rb$size) + 1L
  rb$buf[rb$head] <- id
  rb$n <- min(rb$n + 1L, rb$size)
  rb
}

recent_ids <- function(rb) {
  if (rb$n == 0L) character() else utils::head(rb$buf, rb$n)
}
