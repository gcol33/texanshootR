# Publication-chain state machine.
#
# A shippable run (one that cleared p <= 0.05) opens a *chain*. The
# chain is a sequence of output stages that must be redeemed in order,
# each within its own wall-clock window. Hitting every stage in the
# currently-unlocked prefix awards full XP; running out of time or
# calling an out-of-order stage breaks the chain and stops further XP.
#
# The chain length you may attempt grows with cumulative XP. At day 0
# only `abstract` is in the chain. As XP accrues, the chain grows
# rightward through the publication lifecycle.
#
# State lives on meta:
#   meta$progression$xp                 cumulative XP (integer)
#   meta$progression$length_unlocked    1..length(CHAIN_STAGES)
#   meta$progression$chains_completed   count of fully-completed chains
#   meta$active_chain                   NULL or live-chain state
#     $run_id            which run is being published
#     $stage_idx         1-based index of the NEXT-due stage
#     $deadline          POSIXct, current window expiry
#     $completed_stages  character vector of stages landed so far
#     $opened_at         POSIXct, when shoot() opened this chain
#
# The active chain persists across R commands because meta is on disk.
# When the next call comes in we recompute window state from
# Sys.time() vs $deadline; an expired chain is treated as broken.

CHAIN_STAGES <- c(
  "abstract",
  "manuscript",
  "presentation",
  "reviewer_response",
  "graphical_abstract",
  "funding"
)

# Cumulative XP at which each chain length unlocks. Index = length.
# length 1 is free; lengths 2..6 cost more each step.
CHAIN_XP_THRESHOLDS <- c(
  0L,    # length 1: abstract (free)
  5L,    # length 2: + manuscript
  15L,   # length 3: + presentation
  30L,   # length 4: + reviewer_response
  55L,   # length 5: + graphical_abstract
  90L    # length 6: + funding
)

# Career-tier label derived from how many stages are unlocked.
CHAIN_TIER_BY_LENGTH <- c(
  "Junior Researcher",   # length 1
  "Postdoc",             # length 2
  "Postdoc",             # length 3
  "Senior Scientist",    # length 4
  "Senior Scientist",    # length 5
  "PI"                   # length 6
)

# Default per-stage window (seconds). Override with
# `options(texanshootR.chain_window = N)`.
CHAIN_WINDOW_DEFAULT <- 30

chain_window_seconds <- function() {
  as.numeric(getOption("texanshootR.chain_window",
                        CHAIN_WINDOW_DEFAULT))
}

stage_index <- function(stage) {
  match(stage, CHAIN_STAGES)
}

length_unlocked_for_xp <- function(xp) {
  xp <- as.integer(xp %||% 0L)
  out <- 1L
  for (i in seq_along(CHAIN_XP_THRESHOLDS)) {
    if (xp >= CHAIN_XP_THRESHOLDS[i]) out <- i
  }
  out
}

career_level_for_length <- function(length_unlocked) {
  i <- max(1L, min(as.integer(length_unlocked %||% 1L),
                    length(CHAIN_TIER_BY_LENGTH)))
  CHAIN_TIER_BY_LENGTH[[i]]
}

# Read the progression block from meta, filling defaults for missing
# fields so callers can rely on a stable shape.
progression_of <- function(meta) {
  p <- meta$progression %||% list()
  list(
    xp                = as.integer(p$xp %||% 0L),
    length_unlocked   = as.integer(p$length_unlocked %||% 1L),
    chains_completed  = as.integer(p$chains_completed %||% 0L),
    chains_broken     = as.integer(p$chains_broken %||% 0L)
  )
}

set_progression <- function(meta, prog) {
  meta$progression <- prog
  meta$career_level <- career_level_for_length(prog$length_unlocked)
  meta
}

# Award XP and recompute length_unlocked. Returns the updated meta
# plus attributes capturing what changed (used for the unlock banner).
award_xp <- function(meta, n) {
  prog   <- progression_of(meta)
  prev_l <- prog$length_unlocked
  prog$xp <- prog$xp + as.integer(n)
  new_l  <- length_unlocked_for_xp(prog$xp)
  prog$length_unlocked <- new_l
  meta <- set_progression(meta, prog)
  attr(meta, "xp_awarded")    <- as.integer(n)
  attr(meta, "length_grew")   <- new_l > prev_l
  attr(meta, "new_length")    <- new_l
  attr(meta, "prev_length")   <- prev_l
  meta
}

# Open a new chain on a shippable run. Aborts any existing chain
# (the player committed to a fresh run before redeeming the previous
# one). The first window starts now.
open_chain <- function(meta, run_id) {
  prog <- progression_of(meta)
  if (!is.null(meta$active_chain)) {
    prog$chains_broken <- prog$chains_broken + 1L
    meta <- set_progression(meta, prog)
  }
  meta$active_chain <- list(
    run_id            = run_id,
    stage_idx         = 1L,
    deadline          = as.numeric(Sys.time()) + chain_window_seconds(),
    completed_stages  = character(),
    opened_at         = as.numeric(Sys.time())
  )
  meta
}

close_chain <- function(meta, reason = c("complete", "broken")) {
  reason <- match.arg(reason)
  prog <- progression_of(meta)
  if (reason == "complete") {
    prog$chains_completed <- prog$chains_completed + 1L
  } else {
    prog$chains_broken <- prog$chains_broken + 1L
  }
  meta <- set_progression(meta, prog)
  meta$active_chain <- NULL
  meta
}

# Advance the chain past `stage` (the just-landed stage). If we've
# reached the end of the unlocked prefix, close the chain as
# complete and award the chain-bonus XP. Otherwise, push the deadline
# out one window and move on.
advance_chain <- function(meta, stage) {
  ac <- meta$active_chain
  if (is.null(ac)) return(meta)
  prog <- progression_of(meta)
  ac$completed_stages <- c(ac$completed_stages, stage)
  ac$stage_idx <- ac$stage_idx + 1L
  ac$deadline  <- as.numeric(Sys.time()) + chain_window_seconds()
  meta$active_chain <- ac
  meta <- award_xp(meta, 1L)

  # If the next stage index is beyond what the player has unlocked,
  # the chain is complete: bonus XP, close it.
  prog <- progression_of(meta)
  if (ac$stage_idx > prog$length_unlocked) {
    meta <- award_xp(meta, prog$length_unlocked)
    meta <- close_chain(meta, "complete")
    attr(meta, "chain_completed") <- TRUE
  }
  meta
}

# Window-remaining helper. Returns NA if no active chain, else
# seconds remaining (may be negative if expired).
chain_seconds_remaining <- function(meta) {
  ac <- meta$active_chain
  if (is.null(ac)) return(NA_real_)
  as.numeric(ac$deadline) - as.numeric(Sys.time())
}

# Reasons a stage call can fail. Each returns a structured condition
# we can both print and use in tests.
tx_chain_error <- function(reason, message, fields = list()) {
  structure(
    class = c("tx_chain_error", "error", "condition"),
    c(list(message = message, call = NULL, reason = reason), fields)
  )
}

# The single check called from every output_*.R generator. Verifies
# everything about chain state. On pass, advances the chain and
# persists the new meta. On any failure, signals tx_chain_error with
# a reason code so callers (and tests) can branch on it.
require_chain_stage <- function(stage, run) {
  if (!stage %in% CHAIN_STAGES) {
    stop(sprintf("`%s` is not a known chain stage.", stage), call. = FALSE)
  }
  meta <- read_meta() %||% default_meta(save_dir() %||% tempfile())
  prog <- progression_of(meta)

  s_idx <- stage_index(stage)
  if (s_idx > prog$length_unlocked) {
    next_thr <- CHAIN_XP_THRESHOLDS[min(s_idx,
                                          length(CHAIN_XP_THRESHOLDS))]
    msg <- format_locked_stage(stage, prog$xp, next_thr)
    stop(tx_chain_error("not_unlocked", msg,
                        list(stage = stage, xp = prog$xp,
                             xp_needed = next_thr)))
  }

  ac <- meta$active_chain
  if (is.null(ac)) {
    msg <- paste0(stage, "() requires:\n",
                  "an active publication chain\n\n",
                  "Active chain:\n",
                  "none. Run shoot() and clear p <= 0.05 first.")
    stop(tx_chain_error("no_active_chain", msg,
                        list(stage = stage)))
  }

  remaining <- as.numeric(ac$deadline) - as.numeric(Sys.time())
  if (remaining < 0) {
    completed <- ac$completed_stages
    completed_str <- if (length(completed))
                       paste(completed, collapse = ", ") else "none"
    meta <- close_chain(meta, "broken")
    write_meta(meta)
    msg <- paste0(stage, "() failed:\n",
                  "window expired\n\n",
                  "Completed before timeout:\n",
                  completed_str)
    stop(tx_chain_error("window_expired", msg,
                        list(stage = stage,
                             completed = completed)))
  }

  if (!identical(run$run_id, ac$run_id)) {
    msg <- paste0(stage, "() failed:\n",
                  "this run is not the active publication\n\n",
                  "Active run:\n",
                  ac$run_id)
    stop(tx_chain_error("wrong_run", msg,
                        list(stage = stage,
                             expected = ac$run_id,
                             provided = run$run_id)))
  }

  due_stage <- CHAIN_STAGES[[ac$stage_idx]]
  if (!identical(stage, due_stage)) {
    msg <- paste0(stage, "() failed:\n",
                  "next due stage is ", due_stage, "\n\n",
                  "Completed so far:\n",
                  if (length(ac$completed_stages))
                    paste(ac$completed_stages, collapse = ", ")
                  else "none")
    stop(tx_chain_error("wrong_stage", msg,
                        list(stage = stage, expected = due_stage,
                             completed = ac$completed_stages)))
  }

  # All checks pass. Caller will write its file, then must call
  # advance_chain_after_stage() to persist the advance. (Splitting
  # the check from the persist lets the caller fail without
  # advancing.)
  invisible(list(meta = meta, ok = TRUE))
}

# Called by an output generator after it has successfully produced
# its file. Persists chain advance + XP. Returns the new meta (with
# attributes describing the transition for the post-stage HUD).
advance_chain_after_stage <- function(stage) {
  meta <- read_meta()
  if (is.null(meta) || is.null(meta$active_chain)) return(invisible(NULL))
  meta <- advance_chain(meta, stage)
  write_meta(meta)
  invisible(meta)
}

format_locked_stage <- function(stage, xp, xp_needed) {
  paste0(stage, "() requires:\n",
         xp_needed, " XP (chain length ", stage_index(stage), ")\n\n",
         "Current XP:\n",
         xp)
}

# Public-facing one-line HUD for the active chain. Used by the print
# method of tx_run and by progress().
format_chain_hud <- function(meta) {
  ac <- meta$active_chain
  if (is.null(ac)) return(NULL)
  prog <- progression_of(meta)
  due <- CHAIN_STAGES[[ac$stage_idx]]
  remaining <- as.numeric(ac$deadline) - as.numeric(Sys.time())
  if (remaining < 0) {
    return(sprintf("Active chain: window expired on %s.", due))
  }
  sprintf(
    "Active chain: next %s() in %.0fs   [%d / %d done]",
    due, remaining,
    length(ac$completed_stages),
    prog$length_unlocked
  )
}
