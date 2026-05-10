#' Run an exploratory model search
#'
#' Fits a battery of candidate specifications across predictor subsets,
#' transformations, interactions, outlier-removal seeds, and subgroup
#' seeds. Returns a `tx_run` object summarising the search and the
#' highlighted specification.
#'
#' @param df A data frame.
#' @param formula Optional formula. When `NULL`, the first numeric
#'   column is used as the outcome and all other columns as predictors.
#' @param seed Integer seed. When `NULL`, a random seed is generated
#'   and stored on the returned run.
#' @param theatrical Logical. When `TRUE` and the session is
#'   interactive, opens the three-zone TUI and renders progress;
#'   when `FALSE`, the search runs silently.
#' @param escalate Logical. Allow the derived-metric escalation phase
#'   when the main search produces no spec with p <= 0.05. Defaults to
#'   `TRUE`; the package leans into the parody.
#' @param depth `"default"` for the full search; `"demo"` for a
#'   single-fit smoke test used in CRAN-safe examples.
#' @param ... Reserved for future arguments.
#'
#' @return A `tx_run` object (returned invisibly).
#' @export
shoot <- function(df,
                  formula    = NULL,
                  seed       = NULL,
                  theatrical = TRUE,
                  escalate   = TRUE,
                  depth      = c("default", "demo"),
                  ...) {
  depth <- match.arg(depth)
  if (!is.data.frame(df)) stop("`df` must be a data frame.", call. = FALSE)

  if (is.null(seed)) seed <- sample.int(.Machine$integer.max - 1L, 1L)
  seed <- as.integer(seed)
  set.seed(seed)

  parsed <- parse_shoot_inputs(df, formula, depth)
  outcome    <- parsed$outcome
  predictors <- parsed$predictors

  ui   <- NULL
  open_tui <- isTRUE(theatrical) && interactive() && depth != "demo"
  if (open_tui) ui <- ui_session_open()

  # Internal pacing. Theatrical runs are wall-clock paced so the show
  # has room to breathe; silent runs just exhaust the grid up to a cap.
  # `texanshootR.budget` is an undocumented escape hatch for tests.
  budget <- getOption("texanshootR.budget",
                      if (open_tui) 40L else 86400L)

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
    mascot            = "composed",
    last_loading_at   = 0,
    last_blip_at      = 0,
    last_heartbeat_at = 0,
    tick              = 0L,
    stopped_early     = FALSE,
    resolved_at_progress = NA_real_,
    ultra_rare_seen   = FALSE
  )

  # Demo path: one fit, no theatre, fast.
  if (depth == "demo") {
    grid <- list(list(
      subset = utils::head(predictors, 2L),
      transforms = stats::setNames(rep("identity", min(2L, length(predictors))),
                                    utils::head(predictors, 2L)),
      interactions = character(),
      outlier_seed = "none",
      subgroup_seed = "none"
    ))
    one <- fit_spec(df, outcome, grid[[1]])
    state$spec_count <- 1L
    return(invisible(finalize_run(state, df, outcome, predictors,
                                   results = list(one), grid = grid,
                                   ui = NULL, open_tui = FALSE)))
  }

  # Build the search grid.
  cap <- spec_cap_for_budget(budget)
  grid <- build_search_grid(df, outcome, predictors, cap = cap)
  for (i in seq_along(grid)) grid[[i]]$subset_full <- paste(predictors, collapse = ",")

  results <- vector("list", length(grid))

  end_time <- state$started + budget
  for (i in seq_along(grid)) {
    if (Sys.time() >= end_time) break

    results[[i]] <- fit_spec(df, outcome, grid[[i]])
    state$spec_count <- state$spec_count + 1L

    if (open_tui) {
      progress <- i / length(grid)
      state <- maybe_animate(state, ui, progress, results, outcome)
    }

    # Optional stopping for highlight selection only.
    if (!state$stopped_early) {
      best_p <- min(vapply(results[seq_len(i)], function(r) r$p_value %||% NA_real_,
                            numeric(1)), na.rm = TRUE)
      if (is.finite(best_p) && best_p <= 0.05) {
        state$stopped_early <- TRUE
        state$resolved_at_progress <- i / length(grid)
      }
    }
  }
  results <- Filter(Negate(is.null), results)

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
  # composite response and refit a small grid.
  best_p <- if (length(results)) {
    min(vapply(results, function(r) r$p_value %||% NA_real_, numeric(1)),
        na.rm = TRUE)
  } else NA_real_

  if (isTRUE(escalate) && (is.na(best_p) || best_p > 0.05) &&
      Sys.time() < end_time) {
    derived <- apply_derived(df, outcome, predictors)
    if (!is.null(derived)) {
      state$derived_used <- TRUE
      d2 <- derived$df
      grid2 <- build_search_grid(d2, outcome, predictors, cap = max(50L, cap %/% 10L))
      for (g in grid2) {
        if (Sys.time() >= end_time) break
        r <- fit_spec(d2, outcome, g)
        if (!is.null(r)) {
          r$derived <- derived$info
          results[[length(results) + 1L]] <- r
          state$spec_count <- state$spec_count + 1L
        }
      }
    }
  }

  finalize_run(state, df, outcome, predictors, results, grid, ui, open_tui)
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

finalize_run <- function(state, df, outcome, predictors, results, grid,
                          ui, open_tui) {
  highlight <- choose_highlight(results)

  rev <- reviewer_roll(reviewer_resistance =
                          state$modifiers$reviewer_resistance %||% 0)
  state$modifiers$reviewer_resistance <-
    (state$modifiers$reviewer_resistance %||% 0) + rev$resist_delta

  flags <- detect_causal_flags(df, outcome, predictors, highlight)

  run_id <- format(state$started, "%Y%m%d-%H%M%S")
  search_summary <- list(
    subset_count      = length(unique(vapply(grid, function(g) paste(g$subset, collapse = ","), ""))),
    transform_count   = length(unique(vapply(grid, function(g) paste(names(g$transforms), g$transforms, sep = ":", collapse = ","), ""))),
    interaction_count = length(unique(unlist(lapply(grid, function(g) g$interactions)))),
    outlier_count     = length(unique(vapply(grid, function(g) g$outlier_seed, ""))),
    subgroup_count    = length(unique(vapply(grid, function(g) g$subgroup_seed, "")))
  )

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
    displayed_message_ids = state$displayed_messages,
    grid_hash         = digest_grid(grid),
    search            = search_summary,
    stopped_early     = state$stopped_early,
    resolved_at_progress = state$resolved_at_progress,
    ultra_rare_seen   = state$ultra_rare_seen,
    collider_conditioned     = flags$collider_conditioned,
    omitted_variable_flagged = flags$omitted_variable_flagged,
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
  write_meta(meta)
  write_run_record(run)
  write_recent_buffer(state$recent_buf)

  # Achievement evaluation.
  run$achievements_awarded <- award_and_equip(run, meta)

  if (open_tui) ui_session_close(ui)

  if (isTRUE(attr(meta, "promoted"))) {
    say(sprintf("Career advancement detected. %s -> %s",
                attr(meta, "from_level"), attr(meta, "to_level")))
  }

  invisible(run)
}

# Heuristic detection for the parody's causal-overreach achievements.
#
# collider_conditioned: highlighted spec includes an interaction term
#   AND the product of that pair is more strongly correlated with the
#   outcome than either component alone. That's the textbook fingerprint
#   of conditioning on a path that opens by including a descendant
#   interaction.
#
# omitted_variable_flagged: at least one dropped predictor has |cor|
#   with the outcome above the OMITTED_THRESHOLD - i.e., the spec
#   pruned a variable that obviously matters.
detect_causal_flags <- function(df, outcome, predictors, highlight) {
  out <- list(collider_conditioned = FALSE,
              omitted_variable_flagged = FALSE)
  if (is.null(highlight)) return(out)
  if (!(outcome %in% names(df)) || !is.numeric(df[[outcome]])) return(out)
  y <- df[[outcome]]

  # Collider: interaction terms in the highlighted formula.
  formula_str <- highlight$formula %||% ""
  rhs <- sub("^[^~]*~", "", formula_str)
  int_terms <- regmatches(rhs, gregexpr("[A-Za-z_.][A-Za-z0-9_.]*\\s*:\\s*[A-Za-z_.][A-Za-z0-9_.]*", rhs))[[1]]
  for (tm in int_terms) {
    parts <- trimws(strsplit(tm, ":", fixed = TRUE)[[1]])
    if (length(parts) != 2L) next
    if (!all(parts %in% names(df))) next
    a <- df[[parts[1]]]; b <- df[[parts[2]]]
    if (!is.numeric(a) || !is.numeric(b)) next
    prod <- a * b
    if (sd(prod, na.rm = TRUE) == 0) next
    cab <- abs(suppressWarnings(stats::cor(prod, y, use = "complete.obs")))
    ca  <- abs(suppressWarnings(stats::cor(a, y, use = "complete.obs")))
    cb  <- abs(suppressWarnings(stats::cor(b, y, use = "complete.obs")))
    if (is.finite(cab) && is.finite(ca) && is.finite(cb) &&
        cab > max(ca, cb) + 0.05) {
      out$collider_conditioned <- TRUE
      break
    }
  }

  # Omitted variable: dropped predictor with strong marginal cor.
  dropped <- highlight$dropped %||% character()
  for (v in dropped) {
    if (!(v %in% names(df))) next
    if (!is.numeric(df[[v]])) next
    cv <- abs(suppressWarnings(stats::cor(df[[v]], y, use = "complete.obs")))
    if (is.finite(cv) && cv >= 0.40) {
      out$omitted_variable_flagged <- TRUE
      break
    }
  }
  out
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

digest_grid <- function(grid) {
  s <- vapply(grid, function(g) {
    paste(c(paste(g$subset, collapse = ","),
            paste(names(g$transforms), g$transforms, sep = ":", collapse = ","),
            paste(g$interactions, collapse = ","),
            g$outlier_seed,
            g$subgroup_seed),
          collapse = "|")
  }, "")
  paste(length(grid), substr(rlang_md5(paste(s, collapse = "\n")), 1, 12),
        sep = "-")
}

# Cheap djb2-style string hash to avoid a digest dep. We only use it
# to fingerprint the search grid in the run record - collisions are
# acceptable.
rlang_md5 <- function(x) {
  bytes <- as.integer(charToRaw(x))
  h <- 5381                       # double; avoids 32-bit overflow
  for (b in bytes) {
    h <- (h * 33 + b) %% 2147483647
  }
  sprintf("%08x", as.integer(h))
}
