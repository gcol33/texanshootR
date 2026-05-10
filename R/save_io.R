# Save / load helpers. Hybrid storage:
#   meta.json              human-readable career meta + hidden modifiers
#   runs/<run_id>.rds      lightweight run records (no raw data frames)
#   achievements.rds       award state
#   wardrobe.rds           equipped + unlocked cosmetics
#   recent_messages.rds    ring buffer for message recency
#   sightings.rds          ultra-rare event log
#
# All writes go through `save_dir()`, which respects
# `options(texanshootR.save_dir)` (test override) and
# `options(texanshootR.save_enabled)` (global opt-out).

SAVE_VERSION <- 1L

# Resolved save directory. NULL = persistence disabled.
save_dir <- function() {
  if (!isTRUE(opt("texanshootR.save_enabled"))) return(NULL)
  override <- getOption("texanshootR.save_dir", NULL)
  if (!is.null(override)) return(override)
  tools::R_user_dir("texanshootR", which = "data")
}

# Ensure save dir exists with consent.
ensure_save_dir <- function() {
  d <- save_dir()
  if (is.null(d)) return(NULL)
  if (!dir.exists(d)) {
    if (!consent_ok(d)) return(NULL)
    dir.create(file.path(d, "runs"), recursive = TRUE, showWarnings = FALSE)
    write_meta(default_meta(d))
  } else {
    if (!file.exists(file.path(d, "meta.json"))) {
      write_meta(default_meta(d))
    }
    if (!dir.exists(file.path(d, "runs"))) {
      dir.create(file.path(d, "runs"), showWarnings = FALSE)
    }
  }
  d
}

default_meta <- function(d) {
  list(
    save_version = SAVE_VERSION,
    career_id    = paste0("career-", format(Sys.time(), "%Y%m%d-%H%M%S")),
    consent      = TRUE,
    consent_path = d,
    created_at   = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    last_seen    = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    career_level = "Junior Researcher",
    runs_count   = 0L,
    # Hidden score components - never exposed in print methods.
    hidden = list(
      models_fit            = 0,
      output_complexity     = 0,
      reviewer_resistance   = 0,
      narrative_strength    = 0,
      h_index_proxy         = 0,
      presentation_quality  = 0
    ),
    favorite_method     = NA_character_,
    most_removed_var    = NA_character_,
    events_witnessed    = list()
  )
}

read_meta <- function() {
  d <- save_dir()
  if (is.null(d)) return(NULL)
  f <- file.path(d, "meta.json")
  if (!file.exists(f)) return(NULL)
  meta <- jsonlite::read_json(f, simplifyVector = TRUE,
                              simplifyDataFrame = FALSE,
                              simplifyMatrix = FALSE)
  meta <- migrate_meta(meta)
  meta
}

write_meta <- function(meta) {
  d <- save_dir()
  if (is.null(d)) return(invisible(NULL))
  if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
  meta$last_seen <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  jsonlite::write_json(
    meta,
    path = file.path(d, "meta.json"),
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null"
  )
  invisible(meta)
}

write_run_record <- function(run) {
  d <- ensure_save_dir()
  if (is.null(d)) return(invisible(NULL))
  rec <- run_to_record(run)  # privacy-stripped
  saveRDS(rec, file.path(d, "runs", paste0(rec$run_id, ".rds")))
  invisible(rec)
}

list_run_records <- function() {
  d <- save_dir()
  if (is.null(d) || !dir.exists(file.path(d, "runs"))) return(character())
  list.files(file.path(d, "runs"), pattern = "\\.rds$", full.names = TRUE)
}

read_run_record <- function(run_id) {
  d <- save_dir()
  if (is.null(d)) return(NULL)
  f <- file.path(d, "runs", paste0(run_id, ".rds"))
  if (!file.exists(f)) return(NULL)
  readRDS(f)
}

# Convert a tx_run object to a privacy-stripped record. We keep
# variable *names* and *summary stats*, never the raw data frame.
# Achievement-relevant flags are preserved so the record can be
# re-evaluated when output generators run later.
run_to_record <- function(run) {
  list(
    run_id            = run$run_id,
    seed              = run$seed,
    timestamp         = run$timestamp,
    package_version   = run$package_version,
    df_meta           = run$df_meta,
    formula           = run$formula,
    spec_count        = run$spec_count,
    highlighted_spec  = run$highlighted_spec,
    derived_used      = run$derived_used,
    reviewer_outcome  = run$reviewer_outcome,
    events            = run$events,
    modifiers         = run$modifiers,
    displayed_message_ids = run$displayed_message_ids,
    grid_hash         = run$grid_hash,
    search            = run$search,
    stopped_early     = run$stopped_early %||% FALSE,
    resolved_at_progress = run$resolved_at_progress,
    ultra_rare_seen   = isTRUE(run$ultra_rare_seen),
    peak_mascot       = run$peak_mascot %||% "composed",
    harked            = isTRUE(run$harked),
    outputs_generated = run$outputs_generated %||% character(),
    outputs_generated_files = run$outputs_generated_files %||% character(),
    achievements_awarded    = run$achievements_awarded %||% character()
  )
}

read_recent_buffer <- function() {
  d <- save_dir()
  if (is.null(d)) return(recent_buffer())
  f <- file.path(d, "recent_messages.rds")
  if (!file.exists(f)) return(recent_buffer())
  readRDS(f)
}

write_recent_buffer <- function(rb) {
  d <- ensure_save_dir()
  if (is.null(d)) return(invisible(NULL))
  saveRDS(rb, file.path(d, "recent_messages.rds"))
  invisible(rb)
}

read_achievements_state <- function() {
  d <- save_dir()
  if (is.null(d)) return(list())
  f <- file.path(d, "achievements.rds")
  if (!file.exists(f)) return(list())
  readRDS(f)
}

write_achievements_state <- function(state) {
  d <- ensure_save_dir()
  if (is.null(d)) return(invisible(NULL))
  saveRDS(state, file.path(d, "achievements.rds"))
  invisible(state)
}

read_wardrobe_state <- function() {
  d <- save_dir()
  if (is.null(d)) return(default_wardrobe())
  f <- file.path(d, "wardrobe.rds")
  if (!file.exists(f)) return(default_wardrobe())
  readRDS(f)
}

write_wardrobe_state <- function(state) {
  d <- ensure_save_dir()
  if (is.null(d)) return(invisible(NULL))
  saveRDS(state, file.path(d, "wardrobe.rds"))
  invisible(state)
}

default_wardrobe <- function() {
  list(
    equipped = list(hat = NA, poncho = NA, badge = NA, lanyard = NA, cloak = NA),
    unlocked = character()
  )
}

read_sightings <- function() {
  d <- save_dir()
  if (is.null(d)) return(list())
  f <- file.path(d, "sightings.rds")
  if (!file.exists(f)) return(list())
  readRDS(f)
}

append_sighting <- function(s) {
  d <- ensure_save_dir()
  if (is.null(d)) return(invisible(NULL))
  cur <- read_sightings()
  cur[[length(cur) + 1L]] <- s
  saveRDS(cur, file.path(d, "sightings.rds"))
  invisible(cur)
}
