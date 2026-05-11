# Helpers shared across all output_*.R files.

resolve_output_dir <- function(output_dir = NULL) {
  d <- output_dir %||% getOption("texanshootR.output_dir") %||% tempdir()
  if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
  d
}

require_pkg <- function(pkg, fn) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(sprintf("`%s()` requires the '%s' package. Install it with install.packages('%s').",
                 fn, pkg, pkg), call. = FALSE)
  }
  invisible(TRUE)
}

# Filename versioning rotor - if a default-named file already exists
# we step through ("file.ext", "file_v2.ext", "file_v2_REAL.ext",
# "file_v2_REAL_FINAL.ext", "file_v2_REAL_FINAL_clean.ext").
SUFFIX_LADDER <- c("", "_v2", "_v2_REAL", "_v2_REAL_FINAL", "_v2_REAL_FINAL_clean")

versioned_filename <- function(dir, stem, ext, force = FALSE) {
  for (sfx in SUFFIX_LADDER) {
    f <- file.path(dir, paste0(stem, sfx, ".", ext))
    if (!file.exists(f) || isTRUE(force)) return(f)
  }
  # Final fallback: timestamp.
  file.path(dir, sprintf("%s_%s.%s", stem,
                         format(Sys.time(), "%Y%m%d%H%M%S"), ext))
}

status_open  <- function(file) {
  if (isTRUE(opt("texanshootR.quiet"))) return(invisible())
  cat("Saving ", basename(file), "...\n", sep = "")
}
status_close <- function() {
  if (isTRUE(opt("texanshootR.quiet"))) return(invisible())
  cat("Done.\n")
}

# Track that a run produced a given output. Updates the per-run record
# on disk and re-evaluates achievements so output-driven triggers (HARK,
# all-outputs, filename archaeologist, publication pipeline) can fire.
record_output <- function(run, output_name, file) {
  d <- save_dir()
  if (is.null(d)) return(invisible())
  fpath <- file.path(d, "runs", paste0(run$run_id, ".rds"))
  if (!file.exists(fpath)) return(invisible())
  rec <- readRDS(fpath)
  rec$outputs_generated       <- unique(c(rec$outputs_generated %||% character(), output_name))
  rec$outputs_generated_files <- unique(c(rec$outputs_generated_files %||% character(), basename(file)))

  # Carry over runtime-set flags from the in-memory run (e.g. `harked`
  # is set by manuscript() before this call). The on-disk record is
  # what evaluate_achievements() reads.
  for (k in c("harked", "peak_mascot",
              "stopped_early", "resolved_at_progress", "ultra_rare_seen")) {
    v <- run[[k]]
    if (!is.null(v)) rec[[k]] <- v
  }
  saveRDS(rec, fpath)

  meta <- read_meta()
  if (!is.null(meta)) {
    meta$hidden$output_complexity <- (meta$hidden$output_complexity %||% 0) +
      output_complexity_score(output_name)
    write_meta(meta)
    award_and_equip(rec, meta)
  }
  invisible()
}

output_complexity_score <- function(name) {
  switch(name,
    manuscript         = 3,
    presentation       = 2,
    reviewer_response  = 1,
    graphical_abstract = 1,
    funding            = 2,
    1
  )
}

# Map between the CHAIN_STAGES token and the exported generator. Kept
# here so generate_flagged_outputs() doesn't have to know the function
# names directly and so a future stage rename touches one table.
OUTPUT_GENERATORS <- list(
  abstract           = function(run) abstract(run),
  manuscript         = function(run) manuscript(run),
  presentation       = function(run) presentation(run),
  reviewer_response  = function(run) reviewer_response(run),
  graphical_abstract = function(run) graphical_abstract(run),
  funding            = function(run) funding(run)
)

# Auto-generate the chain prefix needed to satisfy the caller's output
# flags. Walks CHAIN_STAGES in order and calls each generator up to the
# highest enabled flag. A locked stage (xp_threshold not yet met) or an
# expired chain window stops the walk -- the generator throws a
# tx_chain_error which we catch and surface as a one-line note. A
# non-shippable run has no active chain, so the first call short-circuits.
generate_flagged_outputs <- function(run, flags) {
  if (!length(flags)) return(run)
  enabled <- vapply(flags, isTRUE, logical(1))
  if (!any(enabled)) return(run)
  if (!isTRUE(run$shippable)) return(run)
  highest <- max(stage_index(names(flags)[enabled]), na.rm = TRUE)
  for (i in seq_len(highest)) {
    stage <- CHAIN_STAGES[[i]]
    gen   <- OUTPUT_GENERATORS[[stage]]
    if (is.null(gen)) next
    res <- tryCatch(gen(run),
                    tx_chain_error = function(e) e,
                    error          = function(e) e)
    if (inherits(res, "tx_chain_error")) {
      say(sprintf("Skipping %s and later stages: %s.", stage, res$reason))
      break
    }
  }
  # Merge the on-disk record's outputs_generated fields back into the
  # in-memory run -- record_output() updates the rds but doesn't touch
  # the local `run` object, so without this re-read shoot() would
  # return a run with empty outputs_generated.
  d <- save_dir()
  if (!is.null(d)) {
    fpath <- file.path(d, "runs", paste0(run$run_id, ".rds"))
    if (file.exists(fpath)) {
      updated <- readRDS(fpath)
      for (k in c("outputs_generated", "outputs_generated_files")) {
        if (!is.null(updated[[k]])) run[[k]] <- updated[[k]]
      }
    }
  }
  run
}
