# Save schema migrators. Each migrate_<from>_to_<to> function takes a
# parsed meta object at version `<from>` and returns one at `<to>`.
# `migrate_meta` chains them.

migrate_meta <- function(meta) {
  v <- meta$save_version %||% 0L
  while (v < SAVE_VERSION) {
    fn <- get0(sprintf("migrate_%d_to_%d", v, v + 1L), inherits = TRUE)
    if (is.null(fn)) {
      stop("No migrator from save_version ", v, " to ", v + 1L, call. = FALSE)
    }
    meta <- fn(meta)
    v <- v + 1L
  }
  meta$save_version <- SAVE_VERSION
  meta
}

# v1 -> v2: introduces the publication-chain progression. The legacy
# multiplicative career_score in `hidden` no longer drives the tier.
# We seed XP and length_unlocked from the prior career_level so
# existing players keep the tier they had earned.
migrate_1_to_2 <- function(meta) {
  legacy <- meta$career_level %||% "Junior Researcher"
  seed_length <- switch(legacy,
    "Junior Researcher" = 1L,
    "Postdoc"           = 2L,
    "Senior Scientist"  = 4L,
    "PI"                = 6L,
    1L
  )
  seed_xp <- CHAIN_XP_THRESHOLDS[seed_length]
  meta$progression <- list(
    xp                = as.integer(seed_xp),
    length_unlocked   = as.integer(seed_length),
    chains_completed  = 0L,
    chains_broken     = 0L
  )
  meta$active_chain <- NULL
  meta
}
