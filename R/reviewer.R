# Reviewer 2 probabilistic encounter.
#
# Outcomes (weighted): accept_enthusiasm | minor | major | reject.
# Weights shift with the run's reviewer_resistance modifier - a more
# resistant run is more likely to land at major / reject (the joke is
# that reviewer 2 is *more* hostile when the work pushes harder).

# Lazily roll the reviewer outcome for `run`. First call rolls using
# the run's accumulated resistance, writes the outcome back to the
# on-disk run record so subsequent reviewer_response() calls reuse the
# same verdict, and applies the resistance delta to the cumulative
# meta$hidden$reviewer_resistance counter (same * 0.05 scaling
# update_career uses for the other hidden modifiers). Returns the run
# with `reviewer_outcome` populated.
#
# A run that already has a non-NA reviewer_outcome is returned
# unchanged -- the verdict is part of the audit trail, not something
# the player can re-roll.
materialize_reviewer_outcome <- function(run) {
  existing <- run$reviewer_outcome %||% NA_character_
  if (!is.na(existing) && nzchar(existing)) return(run)

  resistance <- run$modifiers$reviewer_resistance %||% 0
  rev <- reviewer_roll(reviewer_resistance = resistance)
  run$reviewer_outcome <- rev$outcome

  # Persist on the run record so the next call to reviewer_response()
  # sees the same outcome.
  d <- save_dir()
  if (!is.null(d)) {
    fpath <- file.path(d, "runs", paste0(run$run_id, ".rds"))
    if (file.exists(fpath)) {
      rec <- readRDS(fpath)
      rec$reviewer_outcome <- rev$outcome
      saveRDS(rec, fpath)
    }
  }

  # Cumulative resistance bookkeeping. Mirrors the * 0.05 scaling that
  # update_career() applies to the other run$modifiers entries at
  # finalize time.
  meta <- read_meta()
  if (!is.null(meta)) {
    meta$hidden <- meta$hidden %||% list()
    meta$hidden$reviewer_resistance <-
      (meta$hidden$reviewer_resistance %||% 0) + rev$resist_delta * 0.05
    write_meta(meta)
  }

  run
}

reviewer_roll <- function(reviewer_resistance = 0,
                          career_level = "Junior Researcher") {
  base <- c(accept_enthusiasm = 0.05,
            minor             = 0.30,
            major             = 0.45,
            reject            = 0.20)
  # Higher resistance shifts mass toward the harsh end. Clip to [-0.3, 0.3].
  r <- max(-0.3, min(0.3, reviewer_resistance))
  base["accept_enthusiasm"] <- max(0, base["accept_enthusiasm"] - r * 0.05)
  base["minor"]             <- max(0, base["minor"]             - r * 0.10)
  base["major"]             <- max(0, base["major"]             + r * 0.10)
  base["reject"]            <- max(0, base["reject"]            + r * 0.05)
  base <- base / sum(base)

  outcome <- sample(names(base), size = 1L, prob = base)

  resist_delta <- switch(outcome,
    accept_enthusiasm = 0.10,
    minor             = 0.05,
    major             = -0.05,
    reject            = -0.15
  )

  list(outcome = outcome, resist_delta = resist_delta)
}
