#' Run an exploratory model search
#'
#' Fits a battery of candidate specifications across predictor subsets,
#' transformations, interactions, principled-sounding sample
#' restrictions (complete cases, IQR fences, Cook's D, factor-level
#' restrictions), and outcome-engineering moves (composite indices,
#' residualisation, ratios, within-group z-scoring). Returns a `tx_run`
#' object summarising the search and the highlighted specification.
#'
#' Every shoot has a wall-clock budget (default 30s). Modifiers are
#' pre-rolled at run start by default: shoot() picks a random per-
#' transition loadout (`+glmm`, `+derived_metrics`, ...) and applies
#' each one when the matching mascot state transition fires, extending
#' the deadline by the modifier's per-token bonus (2-5s) and redirecting
#' the search priority. Pass `interactive_modifiers = TRUE` to opt into
#' the readline tactical-prompt window at each transition instead.
#' Without `+derived_metrics`, runs that fail to clear `p <= 0.05`
#' simply lose — the highlighted spec stays above threshold and the
#' output gauntlet does not open.
#'
#' Output flags (`abstract`, `manuscript`, `presentation` /
#' `powerpoint`, `reviewer_response` / `reviewer`, `graphical_abstract`
#' / `graphical`, `funding`) auto-generate the matching file when the
#' run is shippable. The chain prefix needed to reach the highest
#' enabled flag is generated in order: e.g. `presentation = TRUE`
#' produces abstract, manuscript, and presentation.
#'
#' @param df A data frame.
#' @param formula Optional formula. When `NULL`, the first numeric
#'   column is used as the outcome and all other columns as predictors.
#' @param seed Integer seed. When `NULL`, a random seed is generated
#'   and stored on the returned run.
#' @param depth `"default"` for the full search; `"demo"` for a
#'   single-fit smoke test used in CRAN-safe examples.
#' @param terminal When `TRUE`, spawn an external Windows Terminal (or
#'   fallback console window) and run `shoot()` inside it so the full
#'   ANSI multi-zone TUI renders. Useful when calling from RStudio
#'   Console, which only supports `\r`-overwrite and so falls back to
#'   the compressed single-line status. The result is returned to the
#'   calling session as usual. Windows-only.
#' @param interactive_modifiers When `TRUE`, open a `readline()`
#'   tactical-prompt window at each mascot transition so the player
#'   can type `+command` modifiers live. Default `FALSE`: modifiers
#'   are pre-rolled at run start and applied autonomously, which keeps
#'   the in-place animation intact in RStudio's console.
#' @param abstract,manuscript,presentation,reviewer_response,graphical_abstract,funding
#'   Logical flags. When `TRUE` and the run is shippable, auto-generate
#'   the matching output and every chain stage required to reach it.
#'   Default `FALSE`.
#' @param ... Reserved for future arguments and aliases. Recognised
#'   aliases: `powerpoint` (= `presentation`), `reviewer` (=
#'   `reviewer_response`), `graphical` (= `graphical_abstract`).
#'   Internal: `prompt_fn` can be supplied to drive tactical-pause
#'   input non-interactively (tests).
#'
#' @return A `tx_run` object. Returned visibly so the
#'   `print()` banner fires at the prompt; the demo path
#'   (`depth = "demo"`) returns invisibly because there is no banner
#'   worth showing for a one-fit smoke test.
#' @export
shoot <- function(df,
                  formula    = NULL,
                  seed       = NULL,
                  depth      = c("default", "demo"),
                  terminal   = FALSE,
                  interactive_modifiers = FALSE,
                  abstract           = FALSE,
                  manuscript         = FALSE,
                  presentation       = FALSE,
                  reviewer_response  = FALSE,
                  graphical_abstract = FALSE,
                  funding            = FALSE,
                  ...) {
  depth <- match.arg(depth)
  if (!is.data.frame(df)) stop("`df` must be a data frame.", call. = FALSE)

  if (isTRUE(terminal)) {
    return(spawn_shoot_in_terminal(df, formula = formula, seed = seed,
                                    depth = depth, ...))
  }

  # Internal test seam: `prompt_fn` drives tactical_pause without a real
  # terminal. Supplying it implies the test wants the readline path,
  # so it forces interactive_modifiers regardless of the caller's flag.
  dots <- list(...)
  prompt_fn <- dots$prompt_fn
  if (!is.null(prompt_fn)) interactive_modifiers <- TRUE

  # Output-flag aliases. Fold them onto the canonical names so the rest
  # of the function only has to look at one set of names.
  if (isTRUE(dots$powerpoint)) presentation       <- TRUE
  if (isTRUE(dots$reviewer))   reviewer_response  <- TRUE
  if (isTRUE(dots$graphical))  graphical_abstract <- TRUE
  output_flags <- list(
    abstract           = isTRUE(abstract),
    manuscript         = isTRUE(manuscript),
    presentation       = isTRUE(presentation),
    reviewer_response  = isTRUE(reviewer_response),
    graphical_abstract = isTRUE(graphical_abstract),
    funding            = isTRUE(funding)
  )

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
  open_tui <- is_interactive_ui() && depth != "demo"
  if (open_tui) ui <- ui_session_open()

  # Wall-clock budget. Base is 30s. Modifiers extend the deadline when
  # they are injected mid-run via tactical_pause (per-modifier 2-5s).
  # `texanshootR.budget` overrides the base entirely (used by tests
  # to keep the suite fast).
  base_budget <- getOption("texanshootR.budget", 30L)
  budget <- as.integer(base_budget)

  state <- list(
    seed              = seed,
    started           = Sys.time(),
    spec_count        = 0L,
    derived_used      = FALSE,
    modifiers         = list(),
    consumed_modifiers = character(),
    bias              = empty_bias(),
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
    peak_severity     = 0L,
    # Peak severity hit BEFORE the run first cleared p<=0.05. This is
    # the "true" emotional arc: a run that resolved at progress 0.05
    # and then animated through to desperate has peak_severity =
    # desperate (the visible peak) but peak_severity_before_resolve =
    # composed (what the searcher actually felt during the work). The
    # arc summary reads off this so the banner reflects what the run
    # did rather than how long the bar animated.
    peak_severity_before_resolve = 0L
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
                                   modifiers_used = character())))
  }

  # Fit count cap is deterministic in `budget` -- same seed + same
  # budget yields the same trace (and therefore the same grid_hash).
  # Wall-clock pacing happens *on top of* this: once the cap is hit the
  # loop keeps animating the mascot and bar until end_time, but no
  # more specs are fit.
  cap <- spec_cap_for_budget(budget)
  search <- new_search_state(seed_specs(df, outcome, predictors))
  trace   <- list()
  results <- list()
  end_time <- state$started + budget

  # Chain unlock state (frozen at run start). The OUTPUTS row uses this
  # to dim locked stages; career only advances on finalize_run, so a
  # single read here is correct for the full run's worth of redraws.
  meta_snap        <- tryCatch(read_meta(), error = function(e) NULL)
  unlocked_length  <- progression_of(meta_snap %||% list())$length_unlocked

  # Initial render of the two new zones. Subsequent updates happen
  # inside the search loop -- ui_set_* has its own cache so identical
  # consecutive renders are no-ops.
  if (open_tui) {
    ui <- ui_set_modifiers(ui,
            available_modifiers(state$consumed_modifiers, career_level))
    ui <- ui_set_outputs(ui, CHAIN_STAGES, unlocked_length)
  }

  # Modifier mode. By default shoot() pre-rolls a per-transition
  # loadout at run start and applies each pick autonomously when its
  # mascot transition fires -- the readline tactical prompt is opt-in
  # via `interactive_modifiers = TRUE`. Pre-roll is the universal
  # default because RStudio's console cannot host a readline without
  # breaking the in-place \r overwrite (the readline echoes on a fresh
  # line and every subsequent dyn_render overwrites that echo instead
  # of the bar), and there is no upside to leaving the bug-prone path
  # on by default for ANSI terminals either.
  auto_picks <- if (!isTRUE(interactive_modifiers)) {
    pre_roll_modifiers(career_level)
  } else NULL
  if (!is.null(auto_picks) && open_tui) {
    rolled <- Filter(function(x) !is.na(x) && nzchar(x), auto_picks)
    if (length(rolled)) {
      ui_loading(ui, sprintf("Auto-loadout: %s",
                              paste0("+", unlist(rolled), collapse = " ")))
    }
  }

  # Mini-batch the search. Each iteration collects up to `batch_size`
  # specs from next_spec(), fits them in one R/C++ crossing via
  # fit_specs_batch(), and then runs record_result() over the batch
  # so the perturb_queue and dead_subsets state stays sequential. The
  # tradeoff: perturbations queued by spec #i in a batch don't get
  # picked up until the *next* batch — search reactivity is coarsened
  # to batch granularity, but the R/C++ overhead drops accordingly.
  iter <- 0L
  batch_size <- getOption("texanshootR.batch_size", 8L)
  # The loop has two phases under one driver:
  #   (a) fit phase: spec_count < cap. Pull a batch, fit it, record
  #       results. This is the only phase that mutates the trace, so
  #       the trace (and grid_hash) stay seed-deterministic regardless
  #       of how fast or slow the host CPU is.
  #   (b) animate-out phase: cap hit, end_time not yet. Idle briefly
  #       so the mascot + bar can fill the rest of the wall-clock
  #       budget. The user explicitly wants the visible run to last
  #       the full budget rather than truncate at the cap.
  while (Sys.time() < end_time) {
    if (state$spec_count >= cap) {
      # Fitting done; just let the animation finish out the budget.
      # 50ms keeps CPU low and is well under the 300ms mascot tick.
      Sys.sleep(0.05)
      # Keep the bar marching toward 100% during the animate-out phase.
      # Without this the bar would freeze at whatever percent the last
      # fitted spec reached, which on small data is a fraction of the
      # budget -- defeating the "visible run lasts the full budget"
      # intent of the cap-and-animate split.
      if (open_tui) {
        ui_progress(ui, elapsed_progress(state$started, budget))
      }
    } else {
      this_batch <- min(batch_size, cap - state$spec_count)
      batch_specs <- vector("list", this_batch)
      n_picked <- 0L
      for (i in seq_len(this_batch)) {
        pick   <- next_spec(search, df, outcome, predictors, iter)
        search <- pick$state
        spec   <- pick$spec
        if (is.null(spec)) break
        spec$subset_full <- paste(predictors, collapse = ",")
        spec$family <- pick_spec_family(state, career_level, outcome_vec,
                                         family_pool, escalating = FALSE)
        spec <- apply_model_form(spec)
        n_picked <- n_picked + 1L
        batch_specs[[n_picked]] <- spec
        iter <- iter + 1L
      }
      if (n_picked == 0L) {
        # Search exhausted before cap. Fall into the animate-out path
        # rather than breaking, so the bar still fills the budget.
        Sys.sleep(0.05)
      } else {
        length(batch_specs) <- n_picked
        batch_results <- fit_specs_batch(df, outcome, batch_specs)

        for (i in seq_len(n_picked)) {
          spec <- batch_specs[[i]]
          r    <- batch_results[[i]]
          state$current_family <- spec$family$fitter
          state$spec_count <- state$spec_count + 1L
          if (!is.null(r)) {
            results[[length(results) + 1L]] <- r
            trace[[length(trace)     + 1L]] <- spec
            search <- record_result(search, spec, r, df, outcome,
                                    bias_perturb = state$bias$perturb_tag)
          }
          # Per-spec bar redraw. The dedup cache in ui_progress()
          # throttles to actual visual changes (integer-percent
          # transitions), so this is cheap when nothing visible has
          # changed.
          if (open_tui) {
            spec_prog <- elapsed_progress(state$started, budget)
            ui_progress(ui, spec_prog)
          }
        }
      }
    }

    progress <- elapsed_progress(state$started, budget)
    best_p <- suppressWarnings(min(vapply(results,
                                          function(rr) rr$p_value %||% NA_real_,
                                          numeric(1)), na.rm = TRUE))
    if (!is.finite(best_p)) best_p <- NA_real_

    cur <- mascot_state(progress, best_p, escalating = state$derived_used)
    sev <- mascot_severity(cur)
    if (sev > state$peak_severity) state$peak_severity <- sev
    # Track the peak severity hit while the searcher was actually
    # still searching -- i.e., before best_p first crossed 0.05.
    # After resolution the loop keeps animating to fill the budget,
    # and the ladder mechanically walks to desperate at progress
    # >= 0.90, which would over-state the difficulty of every run
    # that resolved early.
    if (!state$stopped_early && sev > state$peak_severity_before_resolve) {
      state$peak_severity_before_resolve <- sev
    }

    prev_mascot <- state$mascot
    if (open_tui) {
      state <- maybe_animate(state, ui, progress, results, outcome)
    } else {
      state$mascot <- cur
    }

    # Mascot transition handler. Two paths:
    #   - Auto (default): apply the pre-rolled pick for this transition,
    #     if any, and announce it via the loading slot. No readline,
    #     no blocking, no broken \r overwrite.
    #   - Interactive: open a tactical_pause window with readline so
    #     the player can type +commands. Opt-in via interactive_modifiers
    #     (or implicitly when a test supplies prompt_fn).
    if (!identical(prev_mascot, state$mascot)) {
      if (!is.null(auto_picks)) {
        pick <- auto_picks[[state$mascot]] %||% NA_character_
        if (!is.na(pick) && nzchar(pick)) {
          state <- apply_modifier(state, pick,
                                  time_bonus_callback = function(secs) {
                                    end_time <<- end_time + secs
                                  })
          if (open_tui) {
            ui_loading(ui, sprintf("+%s applied. %s Deadline +%ds.",
                                    pick,
                                    format_bias_announcement(state$last_modifier$bias),
                                    state$last_modifier$time_bonus))
          }
        }
      } else if (is_interactive_ui() || !is.null(prompt_fn)) {
        pres <- tactical_pause(state, end_time, ui = ui,
                                career_level = career_level,
                                prompt_fn    = prompt_fn)
        state    <- pres$state
        end_time <- pres$end_time
        if (open_tui) {
          ui <- ui_set_modifiers(ui,
                  available_modifiers(state$consumed_modifiers, career_level))
        }
      }
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

  if (isTRUE(state$bias$escalate) && (is.na(best_p) || best_p > 0.05) &&
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
          spec$family <- pick_spec_family(state, career_level,
                                           derived_outcome_vec, family_pool,
                                           escalating = TRUE)
          spec <- apply_model_form(spec)
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

  # Final-frame mascot flash. The in-loop mascot walks the progress
  # ladder regardless of significance -- "resolved" only appears here,
  # as a one-frame snapshot before the TUI tears down, when the run is
  # actually shippable. Done before finalize_run so the user sees the
  # resolved face above whatever chain / promotion banner finalize
  # prints below.
  if (open_tui && length(results)) {
    best_p_final <- suppressWarnings(min(vapply(results,
                    function(rr) rr$p_value %||% NA_real_, numeric(1)),
                    na.rm = TRUE))
    if (is.finite(best_p_final) && best_p_final <= 0.05) {
      ui_set_mascot(ui, "resolved", tick = state$tick + 1L, force = TRUE)
    }
  }

  run <- finalize_run(state, df, outcome, predictors, results, trace, ui,
                      open_tui, modifiers_used = state$consumed_modifiers)

  # Auto-generate any flagged outputs. The chain enforces stage order
  # internally, so we walk CHAIN_STAGES and call each generator the
  # caller asked for plus every earlier stage required to reach it.
  # Skips silently when the run isn't shippable -- the chain isn't open.
  # Returns the run with outputs_generated re-synced from disk.
  run <- generate_flagged_outputs(run, output_flags)

  # Visible return so `shoot(df)` at the prompt auto-prints the
  # tx_run banner (run arc + highlighted spec + chain HUD). Assignment
  # (`run <- shoot(df)`) suppresses auto-print as usual.
  run
}

# Heuristic: about 100 specs per second on a modern CPU for small df.
# Cap at 5000 for safety. The cap is what makes a run's trace
# (and therefore its grid_hash) deterministic in `seed + budget` --
# wall-clock pacing is layered on top to fill the rest of the budget
# with idle animation, not extra fits.
spec_cap_for_budget <- function(budget) {
  min(5000L, max(50L, as.integer(budget * 100L)))
}

# Fraction of the wall-clock budget that has elapsed since the run
# started, clamped to [0, 1]. Drives both the progress bar and the
# mascot state so a fast-fitting df still walks the full emotional arc
# over the budget window.
elapsed_progress <- function(started, budget) {
  min(1, as.numeric(Sys.time() - started, units = "secs") /
         max(1, as.numeric(budget)))
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

  # Heartbeat: every ~300 ms drives the mascot tick. The progress bar
  # is redrawn per-spec inside the batch loop (see shoot.R), where it
  # gets fresh progress on every fit rather than only at batch boundary.
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

  # Progress bar + loading line: every 3-5s. Slow enough to read the
  # longest message (~70 chars) once at a relaxed pace; messages
  # shorter than the budget get the same dwell rather than racing past.
  if (now - state$last_loading_at >= runif(1, 3, 5)) {
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

  # The reviewer roll has moved to reviewer_response(): a run is a
  # finding, not a verdict, so the reviewer only enters when the
  # player tries to turn the finding into publication bureaucracy.
  # `reviewer_outcome` stays NA on the run until materialize_reviewer_outcome()
  # fires from inside the generator.

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
    reviewer_outcome  = NA_character_,
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
    peak_mascot_before_resolve =
      severity_to_state(state$peak_severity_before_resolve),
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
  # Stamp the cumulative run number and the promotion flag onto the run
  # AFTER update_career() has incremented meta$runs_count and set the
  # transient `promoted` attribute (which is lost on write_meta because
  # JSON doesn't carry R attributes). The print banner reads these
  # fields for the "texanshootR :: run 0007" header and the RUN section's
  # `career impact:` line.
  run$run_index <- as.integer(meta$runs_count %||% 0L)
  run$promoted  <- isTRUE(attr(meta, "promoted"))
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
  # Distinct model families the search actually fitted, sorted in
  # registry order so the banner reads "lm glm gam" consistently
  # across runs that happened to hit the same families in different
  # encounter order.
  fams_seen <- unique(vapply(trace,
                              function(s) s$family$fitter %||% "lm",
                              character(1)))
  fams_sorted <- intersect(c("lm", "glm", "gam", "wls", "rlm", "cor",
                              "glmm", "sem"),
                            fams_seen)

  list(
    subset_count      = length(unique(vapply(trace, function(s) paste(s$subset, collapse = ","), ""))),
    transform_count   = length(unique(vapply(trace, function(s) paste(names(s$transforms), s$transforms, sep = ":", collapse = ","), ""))),
    interaction_count = length(unique(unlist(lapply(trace, function(s) s$interactions)))),
    restriction_count = length(unique(vapply(trace,
                          function(s) restriction_digest(s$restriction), ""))),
    outcome_count     = length(unique(vapply(trace,
                          function(s) outcome_construction_digest(s$outcome_construction), ""))),
    model_form_count  = length(unique(vapply(trace,
                          function(s) model_form_digest(s$model_form), ""))),
    families_explored = fams_sorted
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
            restriction_digest(g$restriction),
            outcome_construction_digest(g$outcome_construction),
            model_form_digest(g$model_form)),
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
