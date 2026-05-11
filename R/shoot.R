#' Run an exploratory model search
#'
#' Fits a battery of candidate specifications across predictor subsets,
#' transformations, interactions, outlier-removal seeds, and subgroup
#' seeds. Returns a `tx_run` object summarising the search and the
#' highlighted specification.
#'
#' Every shoot has a wall-clock budget (default 30s). Each modifier
#' you pass extends the budget by 3 seconds and unlocks an extra
#' search behaviour. Without `"derived_metrics"`, runs that fail to
#' clear `p <= 0.05` simply lose — the highlighted spec stays above
#' threshold and the output gauntlet does not open.
#'
#' @param df A data frame.
#' @param formula Optional formula. When `NULL`, the first numeric
#'   column is used as the outcome and all other columns as predictors.
#' @param modifiers Character vector of modifier names. Each adds 3s
#'   to the budget and steers the search. Supported:
#'   \describe{
#'     \item{`"derived_metrics"`}{Allow the desperation phase to
#'       manufacture a composite response when the regular search has
#'       not cleared `p <= 0.05`.}
#'     \item{`"subgroup"`}{Bias the perturbation queue toward subgroup
#'       splits.}
#'     \item{`"outliers"`}{Bias the perturbation queue toward
#'       outlier-exclusion seeds.}
#'     \item{`"gam"`, `"glm"`, `"wls"`, `"cor"`, `"glmm"`, `"sem"`}{
#'       Insist the family selector consider this family for the run,
#'       provided the player has unlocked it.}
#'   }
#' @param seed Integer seed. When `NULL`, a random seed is generated
#'   and stored on the returned run.
#' @param depth `"default"` for the full search; `"demo"` for a
#'   single-fit smoke test used in CRAN-safe examples.
#' @param ... Reserved for future arguments.
#'
#' @return A `tx_run` object (returned invisibly).
#' @export
shoot <- function(df,
                  formula    = NULL,
                  modifiers  = character(),
                  seed       = NULL,
                  depth      = c("default", "demo"),
                  ...) {
  depth <- match.arg(depth)
  if (!is.data.frame(df)) stop("`df` must be a data frame.", call. = FALSE)

  modifiers <- normalise_modifiers(modifiers)
  use_derived <- "derived_metrics" %in% modifiers

  if (is.null(seed)) seed <- sample.int(.Machine$integer.max - 1L, 1L)
  seed <- as.integer(seed)
  set.seed(seed)

  parsed <- parse_shoot_inputs(df, formula, depth)
  outcome    <- parsed$outcome
  predictors <- parsed$predictors

  # Family pool is fixed for the run: career-tier driven, with glm
  # gated on Postdoc+. Resolved once so selection inside the loop is
  # cheap. Outcome vector is captured here too so the selector can
  # check family/outcome compatibility without re-touching df.
  career_level   <- current_career_level()
  family_pool    <- available_families(career_level)
  outcome_vec    <- if (outcome %in% names(df)) df[[outcome]] else NULL

  ui   <- NULL
  open_tui <- interactive() && depth != "demo"
  if (open_tui) ui <- ui_session_open()

  # Wall-clock budget. Base is 30s; each modifier extends by 3s.
  # `texanshootR.budget` overrides the base entirely (used by tests
  # to keep the suite fast).
  base_budget <- getOption("texanshootR.budget", 30L)
  budget <- as.integer(base_budget) +
            MODIFIER_BUDGET_BONUS * length(modifiers)

  state <- list(
    seed              = seed,
    started           = Sys.time(),
    spec_count        = 0L,
    derived_used      = FALSE,
    modifiers         = list(),
    events            = list(),
    displayed_messages = character(),
    recent_buf        = read_recent_buffer(),
    combo_state       = NULL,
    # Tracks which family is currently being fit so the message bank
    # can surface family-specific blips/loadings via the
    # model_family_affinity filter. NULL means "no family chosen yet";
    # universal messages still fire.
    current_family    = NULL,
    mascot            = "composed",
    last_loading_at   = 0,
    last_blip_at      = 0,
    last_heartbeat_at = 0,
    tick              = 0L,
    stopped_early     = FALSE,
    resolved_at_progress = NA_real_,
    ultra_rare_seen   = FALSE,
    peak_severity     = 0L
  )

  # Demo path: one fit, no theatre, fast.
  if (depth == "demo") {
    spec <- new_spec(utils::head(predictors, 2L))
    spec$subset_full <- paste(predictors, collapse = ",")
    spec$family <- select_family(career_level, escalating = FALSE,
                                  outcome   = outcome_vec,
                                  available = family_pool)
    one <- fit_spec(df, outcome, spec)
    state$spec_count <- 1L
    return(invisible(finalize_run(state, df, outcome, predictors,
                                   results = list(one), trace = list(spec),
                                   ui = NULL, open_tui = FALSE,
                                   modifiers_used = modifiers)))
  }

  cap <- spec_cap_for_budget(budget)
  search <- new_search_state(seed_specs(df, outcome, predictors))
  trace   <- list()
  results <- list()
  end_time <- state$started + budget

  # Mini-batch the search. Each iteration collects up to `batch_size`
  # specs from next_spec(), fits them in one R/C++ crossing via
  # fit_specs_batch(), and then runs record_result() over the batch
  # so the perturb_queue and dead_subsets state stays sequential. The
  # tradeoff: perturbations queued by spec #i in a batch don't get
  # picked up until the *next* batch — search reactivity is coarsened
  # to batch granularity, but the R/C++ overhead drops accordingly.
  iter <- 0L
  batch_size <- getOption("texanshootR.batch_size", 8L)
  while (state$spec_count < cap && Sys.time() < end_time) {
    this_batch <- min(batch_size, cap - state$spec_count)
    batch_specs <- vector("list", this_batch)
    n_picked <- 0L
    for (i in seq_len(this_batch)) {
      pick   <- next_spec(search, df, outcome, predictors, iter)
      search <- pick$state
      spec   <- pick$spec
      if (is.null(spec)) break
      spec$subset_full <- paste(predictors, collapse = ",")
      spec$family <- select_family(career_level, escalating = FALSE,
                                    outcome   = outcome_vec,
                                    available = family_pool)
      n_picked <- n_picked + 1L
      batch_specs[[n_picked]] <- spec
      iter <- iter + 1L
    }
    if (n_picked == 0L) break
    length(batch_specs) <- n_picked

    batch_results <- fit_specs_batch(df, outcome, batch_specs)

    for (i in seq_len(n_picked)) {
      spec <- batch_specs[[i]]
      r    <- batch_results[[i]]
      state$current_family <- spec$family$fitter
      state$spec_count <- state$spec_count + 1L
      if (is.null(r)) next
      results[[length(results) + 1L]] <- r
      trace[[length(trace)     + 1L]] <- spec
      search <- record_result(search, spec, r, df, outcome)
    }

    progress <- min(1, max(
      as.numeric(Sys.time() - state$started, units = "secs") /
        max(1, as.numeric(budget)),
      state$spec_count / max(1L, cap)
    ))
    best_p <- suppressWarnings(min(vapply(results,
                                          function(rr) rr$p_value %||% NA_real_,
                                          numeric(1)), na.rm = TRUE))
    if (!is.finite(best_p)) best_p <- NA_real_

    cur <- mascot_state(progress, best_p, escalating = state$derived_used)
    sev <- mascot_severity(cur)
    if (sev > state$peak_severity) state$peak_severity <- sev

    if (open_tui) {
      state <- maybe_animate(state, ui, progress, results, outcome)
    }

    if (!state$stopped_early && is.finite(best_p) && best_p <= 0.05) {
      state$stopped_early <- TRUE
      state$resolved_at_progress <- progress
    }
  }

  # Roll a life event once per run, after the main search.
  ev <- maybe_event(career_level = (read_meta() %||% list())$career_level
                                    %||% "Junior Researcher",
                    run_phase = "late")
  if (!is.null(ev)) {
    state$events[[length(state$events) + 1L]] <- ev
    state <- apply_event_effects(state, ev$effects)
    if (open_tui) ui <- ui_event(ui, ev$event_text, ev$consequence_text)
  }

  # Desperation escalation: if no significant spec, manufacture a
  # composite response and re-flail through wildcard picks against it.
  best_p <- if (length(results)) {
    suppressWarnings(min(vapply(results,
                                function(r) r$p_value %||% NA_real_,
                                numeric(1)), na.rm = TRUE))
  } else NA_real_
  if (!is.finite(best_p)) best_p <- NA_real_

  if (isTRUE(use_derived) && (is.na(best_p) || best_p > 0.05) &&
      Sys.time() < end_time) {
    derived <- apply_derived(df, outcome, predictors)
    if (!is.null(derived)) {
      state$derived_used <- TRUE
      # Escalation forces the desperate face — record it as a peak even
      # if the run never visually got there.
      state$peak_severity <- max(state$peak_severity,
                                 mascot_severity("desperate"))
      d2 <- derived$df
      sub_cap <- max(50L, cap %/% 10L)
      sub_count <- 0L
      derived_outcome_vec <- if (outcome %in% names(d2)) d2[[outcome]] else NULL
      # Escalation has no per-fit state mutation between specs, so we
      # can batch wildcard specs straight through fit_specs_batch.
      while (sub_count < sub_cap && Sys.time() < end_time) {
        this_batch <- min(batch_size, sub_cap - sub_count)
        batch_specs <- vector("list", this_batch)
        n_picked <- 0L
        for (i in seq_len(this_batch)) {
          spec <- wildcard_spec(d2, outcome, predictors)
          if (is.null(spec)) break
          spec$subset_full <- paste(predictors, collapse = ",")
          spec$family <- select_family(career_level, escalating = TRUE,
                                        outcome   = derived_outcome_vec,
                                        available = family_pool)
          n_picked <- n_picked + 1L
          batch_specs[[n_picked]] <- spec
        }
        if (n_picked == 0L) break
        length(batch_specs) <- n_picked

        batch_results <- fit_specs_batch(d2, outcome, batch_specs)
        for (i in seq_len(n_picked)) {
          spec <- batch_specs[[i]]
          r    <- batch_results[[i]]
          state$current_family <- spec$family$fitter
          sub_count <- sub_count + 1L
          state$spec_count <- state$spec_count + 1L
          if (!is.null(r)) {
            r$derived <- derived$info
            results[[length(results) + 1L]] <- r
            trace[[length(trace)     + 1L]] <- spec
          }
        }
      }
    }
  }

  finalize_run(state, df, outcome, predictors, results, trace, ui,
               open_tui, modifiers_used = modifiers)
}

# Modifier vocabulary. Validating up front so a typo doesn't silently
# do nothing. Each modifier costs MODIFIER_BUDGET_BONUS seconds.
SUPPORTED_MODIFIERS <- c(
  "derived_metrics", "subgroup", "outliers",
  "gam", "glm", "wls", "cor", "glmm", "sem"
)
MODIFIER_BUDGET_BONUS <- 3L

normalise_modifiers <- function(mods) {
  if (is.null(mods)) return(character())
  mods <- as.character(mods)
  mods <- mods[nzchar(mods)]
  if (!length(mods)) return(character())
  unknown <- setdiff(mods, SUPPORTED_MODIFIERS)
  if (length(unknown)) {
    stop(sprintf(
      "Unknown modifier(s): %s.\nSupported: %s",
      paste(unknown, collapse = ", "),
      paste(SUPPORTED_MODIFIERS, collapse = ", ")
    ), call. = FALSE)
  }
  unique(mods)
}

# Heuristic: about 100 specs per second on a modern CPU for small df.
# Cap at 5000 for safety.
spec_cap_for_budget <- function(budget) {
  min(5000L, max(50L, as.integer(budget * 100L)))
}

parse_shoot_inputs <- function(df, formula, depth) {
  if (is.null(formula)) {
    is_num <- vapply(df, is.numeric, logical(1))
    if (!any(is_num)) {
      stop("Provide a `formula` or a data frame with at least one ",
           "numeric column.", call. = FALSE)
    }
    outcome <- names(df)[which(is_num)[1]]
    predictors <- setdiff(names(df), outcome)
  } else {
    parts <- as.character(stats::terms(formula))
    outcome <- parts[2]
    predictors <- all.vars(formula)[-1]
  }
  list(outcome = outcome, predictors = predictors)
}

# UI tick: redraw heartbeat / blip / loading according to elapsed
# time since the previous update for each cadence.
maybe_animate <- function(state, ui, progress, results, outcome) {
  now <- as.numeric(Sys.time())
  meta <- read_meta()
  career <- (meta %||% list())$career_level %||% "Junior Researcher"
  flavor <- daily_flavor()
  tag_boost <- flavor$tag_boost

  # Best p so far, for mascot state.
  ps <- vapply(results, function(r) r$p_value %||% NA_real_, numeric(1))
  best_p <- if (length(ps)) suppressWarnings(min(ps, na.rm = TRUE))
            else NA_real_
  if (is.infinite(best_p)) best_p <- NA_real_

  m <- mascot_state(progress, best_p, escalating = state$derived_used)

  # Heartbeat: every ~300 ms.
  if (now - state$last_heartbeat_at >= 0.3) {
    state$tick <- state$tick + 1L
    ui_set_mascot(ui, m, tick = state$tick)
    state$last_heartbeat_at <- now
  }

  # Blip: every 250-500 ms.
  if (now - state$last_blip_at >= runif(1, 0.25, 0.5)) {
    draw <- select_message(
      phase = "blip",
      career = career,
      mascot_state = m,
      model_family = state$current_family,
      recent = recent_ids(state$recent_buf),
      combo_state = state$combo_state,
      tag_boost = tag_boost
    )
    if (!is.null(draw)) {
      ui_blip(ui, draw$text)
      state$recent_buf <- recent_push(state$recent_buf, draw$id)
      state$displayed_messages <- c(state$displayed_messages, draw$id)
      state$combo_state <- if (!is.na(draw$combo_start)) draw$combo_start else NULL
    }
    state$last_blip_at <- now
  }

  # Progress bar + loading line: every 1.5-2.5s.
  if (now - state$last_loading_at >= runif(1, 1.5, 2.5)) {
    draw <- select_message(
      phase = "loading",
      career = career,
      mascot_state = m,
      model_family = state$current_family,
      recent = recent_ids(state$recent_buf),
      combo_state = state$combo_state,
      tag_boost = tag_boost
    )
    if (!is.null(draw)) {
      ui_loading(ui, draw$text)
      state$recent_buf <- recent_push(state$recent_buf, draw$id)
      state$displayed_messages <- c(state$displayed_messages, draw$id)
    }
    ui_progress(ui, progress, "narrative coherence")
    state$last_loading_at <- now
  }

  # Ultra-rare "Glimpse of Tenure": once per run at most, low-prob
  # roll per heartbeat tick. Gated on ANSI + animations.
  if (!isTRUE(state$ultra_rare_seen) &&
      isTRUE(opt("texanshootR.animations")) &&
      runif(1) < getOption("texanshootR.glimpse_rate", 1 / 1000)) {
    ultra_rare_glimpse()
    state$ultra_rare_seen <- TRUE
    append_sighting(list(
      run_seed  = state$seed,
      timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
      progress  = progress
    ))
  }

  # Mascot transition message.
  if (!identical(state$mascot, m)) {
    draw <- select_message(
      phase = "state_transition",
      mascot_state = m,
      recent = recent_ids(state$recent_buf)
    )
    if (!is.null(draw)) {
      ui_loading(ui, draw$text)
      state$recent_buf <- recent_push(state$recent_buf, draw$id)
      state$displayed_messages <- c(state$displayed_messages, draw$id)
    }
    state$mascot <- m
  }

  state
}

finalize_run <- function(state, df, outcome, predictors, results, trace,
                          ui, open_tui, modifiers_used = character()) {
  highlight <- choose_highlight(results)

  rev <- reviewer_roll(reviewer_resistance =
                          state$modifiers$reviewer_resistance %||% 0)
  state$modifiers$reviewer_resistance <-
    (state$modifiers$reviewer_resistance %||% 0) + rev$resist_delta

  run_id <- format(state$started, "%Y%m%d-%H%M%S")
  search_summary <- summarise_trace(trace)
  shippable <- !is.null(highlight) &&
               is.finite(highlight$p_value %||% NA_real_) &&
               (highlight$p_value %||% 1) <= 0.05

  df_meta <- list(
    nrow    = nrow(df),
    ncol    = ncol(df),
    columns = lapply(names(df),
                     function(n) list(name = n,
                                      class = class(df[[n]])[1])),
    summary = numeric_summary(df)
  )

  run <- new_tx_run(
    run_id            = run_id,
    seed              = state$seed,
    timestamp         = format(state$started, "%Y-%m-%dT%H:%M:%S%z"),
    package_version   = as.character(utils::packageVersion("texanshootR")),
    df_meta           = df_meta,
    formula           = if (!is.null(highlight)) highlight$formula else NA_character_,
    spec_count        = state$spec_count,
    highlighted_spec  = highlight,
    derived_used      = state$derived_used,
    reviewer_outcome  = rev$outcome,
    events            = state$events,
    modifiers         = state$modifiers,
    modifiers_used    = modifiers_used,
    shippable         = shippable,
    displayed_message_ids = state$displayed_messages,
    grid_hash         = digest_grid(trace),
    search            = search_summary,
    stopped_early     = state$stopped_early,
    resolved_at_progress = state$resolved_at_progress,
    ultra_rare_seen   = state$ultra_rare_seen,
    peak_mascot       = severity_to_state(state$peak_severity),
    outputs_generated = character(),
    outputs_generated_files = character()
  )

  # Persist.
  meta <- read_meta() %||% default_meta(save_dir() %||% tempfile())
  meta <- update_career(meta, run)
  if (length(state$events)) {
    meta$events_witnessed <- c(meta$events_witnessed %||% list(),
                                lapply(state$events, `[[`, "id"))
  }
  # If the run cleared p <= 0.05, open the publication chain. This
  # aborts any prior chain the player left dangling.
  if (isTRUE(shippable)) {
    meta <- open_chain(meta, run$run_id)
  }
  write_meta(meta)
  write_run_record(run)
  write_recent_buffer(state$recent_buf)

  # Achievement evaluation.
  run$achievements_awarded <- award_and_equip(run, meta)

  if (open_tui) ui_session_close(ui)

  if (isTRUE(shippable)) say_chain_opened(meta)

  if (isTRUE(attr(meta, "promoted"))) {
    say(sprintf("Career advancement detected. %s -> %s",
                attr(meta, "from_level"), attr(meta, "to_level")))
  }

  invisible(run)
}

numeric_summary <- function(df) {
  out <- list()
  for (n in names(df)) {
    x <- df[[n]]
    if (is.numeric(x)) {
      out[[n]] <- list(
        mean = mean(x, na.rm = TRUE),
        sd   = stats::sd(x, na.rm = TRUE),
        min  = suppressWarnings(min(x, na.rm = TRUE)),
        max  = suppressWarnings(max(x, na.rm = TRUE))
      )
    }
  }
  out
}

# Summarise the per-dimension distinct-value counts across the specs
# actually fitted. Replaces the old "grid summary" — there is no grid
# anymore, only the trace of what the shooter ended up trying.
summarise_trace <- function(trace) {
  list(
    subset_count      = length(unique(vapply(trace, function(s) paste(s$subset, collapse = ","), ""))),
    transform_count   = length(unique(vapply(trace, function(s) paste(names(s$transforms), s$transforms, sep = ":", collapse = ","), ""))),
    interaction_count = length(unique(unlist(lapply(trace, function(s) s$interactions)))),
    outlier_count     = length(unique(vapply(trace, function(s) s$outlier_seed, ""))),
    subgroup_count    = length(unique(vapply(trace, function(s) s$subgroup_seed, "")))
  )
}

# Hash the sequence of specs the shooter actually tried. The name
# `digest_grid` is preserved on the run record for backwards
# compatibility, but the input is the trace, not a pre-built grid.
digest_grid <- function(trace) {
  s <- vapply(trace, function(g) {
    paste(c(paste(g$subset, collapse = ","),
            paste(names(g$transforms), g$transforms, sep = ":", collapse = ","),
            paste(g$interactions, collapse = ","),
            g$outlier_seed,
            g$subgroup_seed),
          collapse = "|")
  }, "")
  paste(length(trace), substr(rlang_md5(paste(s, collapse = "\n")), 1, 12),
        sep = "-")
}

# Cheap djb2-style string hash to avoid a digest dep. We only use it
# to fingerprint the search trace in the run record - collisions are
# acceptable.
rlang_md5 <- function(x) {
  bytes <- as.integer(charToRaw(x))
  h <- 5381                       # double; avoids 32-bit overflow
  for (b in bytes) {
    h <- (h * 33 + b) %% 2147483647
  }
  sprintf("%08x", as.integer(h))
}
