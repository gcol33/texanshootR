# tx_run S3 class. Returned (invisibly) by shoot(); consumed by all
# output functions and by `run_log()`.

new_tx_run <- function(...) {
  obj <- list(...)
  class(obj) <- "tx_run"
  obj
}

#' @export
#' @method print tx_run
print.tx_run <- function(x, ...) {
  # Single read of meta -- every section that needs it (career impact,
  # OUTPUTS, PUBLICATION PIPELINE) shares this snapshot. Reading it
  # once also means the banner reflects the state at print time, not
  # the state from when the run finalized (the player may have called
  # output generators or claimed chain stages in between).
  meta <- tryCatch(read_meta(), error = function(e) NULL)

  active_chain <- !is.null(meta) && !is.null(meta$active_chain) &&
                  identical(meta$active_chain$run_id, x$run_id)
  final_state  <- compute_final_state(x, active_chain)

  cat(banner_header(x, final_state), "\n", sep = "")
  cat("\n")

  cat("RUN\n")
  cat("status: ",         format_run_status(x),     "\n", sep = "")
  cat("career impact: ",  format_career_impact(x),  "\n", sep = "")
  cat("mascot state: ",   format_mascot_arc(x, final_state), "\n", sep = "")
  cat("\n")

  cat("FINDING\n")
  for (line in format_finding_block(x)) cat(line, "\n", sep = "")
  cat("\n")

  cat("SEARCH\n")
  for (line in format_search_block(x)) cat(line, "\n", sep = "")
  cat("\n")

  cat("OUTPUTS\n")
  for (line in format_outputs_block(meta)) cat(line, "\n", sep = "")
  cat("\n")

  if (active_chain) {
    cat("PUBLICATION PIPELINE\n")
    for (line in format_pipeline_block(meta)) cat(line, "\n", sep = "")
    cat("\n")
  }

  cat("DATA USED\n")
  for (line in format_data_used_block(x)) cat(line, "\n", sep = "")

  invisible(x)
}

# Banner header: double-rule, face+gun, "texanshootR :: run NNNN",
# double-rule. The face is the resting state at print time -- "polishing"
# when the printed run owns the active chain (the player is between
# stages), "resolved" for any other shippable run, and the run's
# `peak_mascot` for non-shippable runs (the worst face they wore).
banner_header <- function(x, final_state) {
  rule <- strrep("=", 64)
  face <- tryCatch(read_face(final_state), error = function(e) "( o_o)")
  gun  <- "\ufe3b\u30c7\u2550\u4e00  \u00b7"  # canonical frame from inst/ascii/heartbeat.txt
  idx  <- as.integer(x$run_index %||% 0L)
  title <- sprintf("texanshootR :: run %04d", idx)
  paste(rule, paste(face, gun), title, rule, sep = "\n")
}

compute_final_state <- function(x, active_chain) {
  if (isTRUE(x$shippable) && active_chain) return("polishing")
  if (isTRUE(x$shippable))                  return("resolved")
  x$peak_mascot %||% "composed"
}

format_run_status <- function(x) {
  if (isTRUE(x$shippable)) "publishable result identified"
  else                     "no publishable narrative this run"
}

# Three-state career-impact verdict:
#   promoted  = career tier changed during update_career() of this run
#   advanced  = shippable run, chain opened, no promotion
#   stalled   = not shippable (no chain opened, no XP path)
format_career_impact <- function(x) {
  if (!isTRUE(x$shippable))  return("stalled")
  if (isTRUE(x$promoted))    return("promoted")
  "advanced"
}

# `peak_pre_resolve -> final` arc. Tells the actual emotional shape of
# the run: composed->resolved is a glide, desperate->resolved is a save,
# *->desperate is a defeat. The arc reports the RUN's outcome, not the
# player's current stance -- so "polishing" (which is the resting face
# shown atop the banner when the run owns an active chain) maps back
# to "resolved" here. The top-line face still shows polishing.
format_mascot_arc <- function(x, final_state) {
  arc_end <- if (identical(final_state, "polishing")) "resolved" else final_state
  pre <- x$peak_mascot_before_resolve %||% x$peak_mascot %||% "composed"
  sprintf("%s \u2192 %s", pre, arc_end)
}

format_finding_block <- function(x) {
  hs <- x$highlighted_spec
  if (isTRUE(x$shippable) && !is.null(hs)) {
    c(
      hs$formula %||% "",
      sprintf("methods: %s", methods_summary(hs)),
      sprintf("p: %s",
              if (is.finite(hs$p_value %||% NA_real_))
                format.pval(hs$p_value, digits = 3)
              else "\u2014")
    )
  } else if (!is.null(hs)) {
    c(
      sprintf("closest formula: %s", hs$formula %||% ""),
      sprintf("best p seen: %s",
              if (is.finite(hs$p_value %||% NA_real_))
                format.pval(hs$p_value, digits = 3)
              else "\u2014"),
      sprintf("searched: %s specs",
              format(x$spec_count %||% 0L, big.mark = ","))
    )
  } else {
    "(no candidate specifications materialised)"
  }
}

format_search_block <- function(x) {
  fams <- x$search$families_explored %||% character()
  mods <- x$modifiers_used %||% character()
  c(
    sprintf("candidate specifications: %s",
            format(x$spec_count %||% 0L, big.mark = ",")),
    sprintf("model families explored: %s",
            if (length(fams)) paste(fams, collapse = " ") else "none"),
    sprintf("modifiers used: %s",
            if (length(mods)) paste0("+", mods, collapse = " ") else "none")
  )
}

# OUTPUTS block. Walks CHAIN_STAGES against meta$progression$length_unlocked
# and CHAIN_TIER_BY_LENGTH. Unlocked stages stack as `available now:
# fn(run)` with indented continuation. Locked stages render under
# `locked:` in the deadpan two-line format already established for
# `tx_locked` errors: `fn() requires <tier>`. Locked function names are
# width-padded so the requires-column lines up.
format_outputs_block <- function(meta) {
  prog <- progression_of(meta %||% list())
  n_unlocked <- prog$length_unlocked
  stages <- CHAIN_STAGES
  unlocked <- stages[seq_len(n_unlocked)]
  locked   <- if (n_unlocked < length(stages))
                stages[(n_unlocked + 1L):length(stages)]
              else character()

  lines <- character()

  if (length(unlocked)) {
    label   <- "available now: "
    pad     <- strrep(" ", nchar(label))
    lines   <- c(lines,
                 paste0(label, sprintf("%s(run)", unlocked[1])))
    for (s in unlocked[-1]) {
      lines <- c(lines, paste0(pad, sprintf("%s(run)", s)))
    }
  }

  if (length(locked)) {
    if (length(unlocked)) lines <- c(lines, "")
    lines <- c(lines, "locked:")
    # Width-pad fn() column so `requires <tier>` aligns across rows.
    fns <- sprintf("%s()", locked)
    width <- max(nchar(fns))
    for (i in seq_along(locked)) {
      idx <- match(locked[i], stages)
      tier <- CHAIN_TIER_BY_LENGTH[[idx]]
      lines <- c(lines,
                 sprintf("%-*s requires %s", width, fns[i], tier))
    }
  }

  if (!length(lines)) lines <- "(no stages defined)"
  lines
}

# PUBLICATION PIPELINE block. Only invoked when `meta$active_chain$run_id`
# matches the printed run -- i.e. the player is mid-chain on THIS run.
# Reports the next due stage and the claimed / unlocked ratio.
format_pipeline_block <- function(meta) {
  ac   <- meta$active_chain
  prog <- progression_of(meta)
  due  <- CHAIN_STAGES[[ac$stage_idx]]
  done <- length(ac$completed_stages %||% character())
  c(
    sprintf("next available output: %s(run)", due),
    sprintf("pipeline progress: %d / %d", done, prog$length_unlocked)
  )
}

format_data_used_block <- function(x) {
  hs        <- x$highlighted_spec
  excl_rule <- render_restriction_phrase(hs$restriction)
  if (is.na(excl_rule)) excl_rule <- "none"
  outcome_phrase <- if (is.null(hs$outcome_construction)) "none"
                    else if (identical(hs$outcome_construction$kind,
                                        "composite_index")) {
                      vars <- hs$outcome_construction$vars %||% character()
                      if (length(vars)) sprintf("composite outcome from %s",
                                                 oxford_join(vars))
                      else "none"
                    } else {
                      o <- render_outcome_phrase(hs$outcome_construction, "")
                      if (is.na(o)) "none" else o
                    }
  rows_excluded <- as.integer(hs$rows_excluded %||% 0L)

  stored <- !is.null(save_dir()) && isTRUE(opt("texanshootR.save_enabled"))

  c(
    sprintf("exclusion rule: %s", excl_rule),
    sprintf("rows excluded: %s", format(rows_excluded, big.mark = ",")),
    sprintf("outcome construction: %s", outcome_phrase),
    sprintf("derived metrics: %s",
            if (isTRUE(x$derived_used)) "enabled" else "disabled"),
    sprintf("search seed: %s",
            if (!is.null(x$seed)) as.character(x$seed) else "none"),
    sprintf("run stored: %s", if (isTRUE(stored)) "yes" else "no")
  )
}

# Parser-based formula split. Kept for callers that need the
# outcome / predictors decomposition (e.g. future banner extensions).
# Returns empty strings on parse failure.
parse_formula_parts <- function(formula_str) {
  f <- tryCatch(stats::as.formula(formula_str), error = function(e) NULL)
  if (is.null(f) || length(f) < 3L) {
    return(list(outcome = "", predictors = ""))
  }
  list(
    outcome    = paste(deparse(f[[2]]), collapse = " "),
    predictors = paste(deparse(f[[3]]), collapse = " ")
  )
}

#' @export
#' @method summary tx_run
summary.tx_run <- function(object, ...) {
  print(object)
  cat("\n")
  cat("Search dimensions explored:\n")
  s <- object$search
  if (!is.null(s)) {
    cat(sprintf("  Predictor subsets:     %d\n",  s$subset_count %||% 0))
    cat(sprintf("  Transformations:       %d\n",  s$transform_count %||% 0))
    cat(sprintf("  Interactions:          %d\n",  s$interaction_count %||% 0))
    cat(sprintf("  Restrictions:          %d\n",  s$restriction_count %||% 0))
    cat(sprintf("  Outcome constructions: %d\n",  s$outcome_count %||% 0))
    cat(sprintf("  Model forms:           %d\n",  s$model_form_count %||% 0))
    if (isTRUE(object$derived_used)) {
      cat("  Derived metric:        active\n")
    }
  }
  cat("\nInternal scores (this run):\n")
  m <- object$modifiers %||% list()
  scoreline <- function(name, key) {
    cat(sprintf("  %-22s %+0.2f\n", paste0(name, ":"),
                m[[key]] %||% 0))
  }
  scoreline("Reviewer resistance",    "reviewer_resistance")
  scoreline("Narrative coherence",    "narrative_strength")
  scoreline("Presentation score",     "presentation_pressure")
  if (length(object$achievements_awarded)) {
    cat("\nAchievements awarded this run:\n")
    for (a in object$achievements_awarded) {
      cat("  - ", a, "\n", sep = "")
    }
  }
  invisible(object)
}

#' @export
#' @method format tx_run
format.tx_run <- function(x, ...) {
  paste0("<tx_run ", x$run_id, ">")
}

#' Browse persisted run history
#'
#' Returns a data.frame of recent runs with one row per run. The full
#' run record can be loaded via `attr(run_log(), "records")[[i]]`.
#'
#' @param n Maximum number of runs to list.
#' @return A data.frame with one row per run.
#' @export
run_log <- function(n = 25L) {
  recs <- recent_run_records(n)
  if (length(recs) == 0L) {
    out <- data.frame(
      run_id     = character(),
      timestamp  = character(),
      specs      = integer(),
      best_p     = numeric(),
      reviewer   = character(),
      stringsAsFactors = FALSE
    )
    attr(out, "records") <- list()
    return(out)
  }
  out <- data.frame(
    run_id    = vapply(recs, `[[`, "", "run_id"),
    timestamp = vapply(recs, `[[`, "", "timestamp"),
    specs     = vapply(recs, function(r) as.integer(r$spec_count %||% 0L), integer(1)),
    best_p    = vapply(recs, function(r) {
                  p <- r$highlighted_spec$p_value %||% NA_real_
                  as.numeric(p)
                }, numeric(1)),
    reviewer  = vapply(recs, function(r) r$reviewer_outcome %||% NA_character_, character(1)),
    stringsAsFactors = FALSE
  )
  attr(out, "records") <- recs
  out
}
