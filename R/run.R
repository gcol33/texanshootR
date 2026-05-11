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
  rule <- strrep("-", 48)
  hs   <- x$highlighted_spec
  resolved <- !is.null(hs) && is.finite(hs$p_value %||% NA_real_) &&
              hs$p_value <= 0.05
  active_chain <- tryCatch({
    m <- read_meta()
    !is.null(m) && !is.null(m$active_chain) &&
      identical(m$active_chain$run_id, x$run_id)
  }, error = function(e) FALSE)
  final_state <- if (resolved && active_chain) "polishing"
                 else if (resolved) "resolved"
                 else (x$peak_mascot %||% "composed")
  face <- tryCatch(read_face(final_state), error = function(e) "( o_o)")

  cat(style_header(rule), "\n", sep = "")
  cat(style_header(sprintf("%s  %s", face, run_arc_summary(x, final_state))),
      "\n", sep = "")
  cat(style_header(rule), "\n", sep = "")
  if (isTRUE(x$shippable) && !is.null(hs)) {
    # Present-tense framing: the run is a finding, not a table of
    # diagnostics. The narrative reads off the highlighted formula +
    # subgroup + outlier-drop context, with the p-value inline at the
    # end. Detailed numerics (R^2, n, AIC) stay accessible on
    # `run$highlighted_spec` for programmatic use but don't crowd the
    # banner.
    for (line in format_run_narrative(x)) cat(line, "\n", sep = "")
  } else if (!is.null(hs)) {
    # Non-shippable: show what was tried so the user can see why the
    # search failed.
    cat(sprintf("Closest formula:       %s\n", hs$formula %||% ""))
    cat(sprintf("Best p-value seen:     %s\n",
                if (!is.null(hs$p_value)) format.pval(hs$p_value, digits = 3)
                else "-"))
    cat(sprintf("Searched %s candidates.\n",
                format(x$spec_count %||% 0L, big.mark = ",")))
  }
  if (!is.null(x$reviewer_outcome)) {
    cat(sprintf("Reviewer 2:            %s\n", x$reviewer_outcome))
  }
  if (length(x$events)) {
    cat("Events witnessed:\n")
    for (e in x$events) {
      cat("  - ", e$event_text, " - ", e$consequence_text, "\n", sep = "")
    }
  }
  if (length(x$modifiers_used)) {
    cat(sprintf("Modifiers used:        %s\n",
                paste0("+", x$modifiers_used, collapse = " ")))
  }
  if (isTRUE(x$shippable)) {
    meta <- tryCatch(read_meta(), error = function(e) NULL)
    if (!is.null(meta)) {
      hud <- format_chain_hud(meta)
      if (!is.null(hud)) cat("\n", hud, "\n", sep = "")
    }
  } else {
    cat("\nNo publishable narrative this run -- nothing under p <= 0.05.\n")
  }
  invisible(x)
}

# Plain-English finding line(s) drawn from the highlighted spec. Dry by
# design -- no strength adverbs, no editorial modulation. Just outcome,
# predictors, optional subset / outlier context, and the p-value inline.
# The trailing "Searched N candidate specifications." line is the
# institutional tell: it reframes the regression as the survivor of a
# search, which is the package's whole joke. `strwrap()` reflows to host
# terminal width so wide RStudio sessions read on one line and 80-col
# terminals wrap tightly.
format_run_narrative <- function(x) {
  hs <- x$highlighted_spec
  if (is.null(hs)) return(character())

  parts      <- parse_formula_parts(hs$formula %||% "")
  outcome    <- if (nzchar(parts$outcome))    parts$outcome    else "outcome"
  predictors <- if (nzchar(parts$predictors)) parts$predictors else "predictors"

  context_phrase <- if (!is.na(hs$subgroup %||% NA_character_)) {
    sprintf(" in the %s subset", hs$subgroup)
  } else if ((hs$outliers_dropped %||% 0L) > 0L) {
    n <- hs$outliers_dropped
    sprintf(" after dropping %d outlier%s", n, if (n == 1L) "" else "s")
  } else ""

  pval_str <- if (is.finite(hs$p_value %||% NA_real_))
                format.pval(hs$p_value, digits = 3)
              else "-"

  claim <- sprintf("%s is associated with %s%s (p = %s).",
                   outcome, predictors, context_phrase, pval_str)

  width      <- max(40L, getOption("width", 80L) - 2L)
  body_lines <- strwrap(claim, width = width)

  searched <- sprintf("Searched %s candidate specifications.",
                      format(x$spec_count %||% 0L, big.mark = ","))

  c("Publishable result:", body_lines, searched)
}

# Pull outcome / predictor sides off the formula via the parser, not
# regex on the deparsed string (R formulas are already parse trees;
# regex over them is brittle on multi-term RHS and i:i interactions).
# Returns empty strings on parse failure so the caller can fall back.
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

# System-log-style state transition for the banner header line. Reads
# like an instrument readout rather than a sentence -- dry, institutional,
# uniform format across all runs. Pre-resolve peak goes on the LHS (the
# searcher's actual emotional arc; see `peak_mascot_before_resolve` on
# tx_run for why this is not the all-run peak). Final state on the RHS
# is `resolved` for any shippable run (the active-chain "polishing"
# state is implementation detail), `derived escalation` when the
# escalation branch fired without nominal predictors clearing 0.05, and
# `unresolved` otherwise.
run_arc_summary <- function(x, final_state) {
  arrow <- "→"  # U+2192 RIGHTWARDS ARROW
  if (final_state %in% c("resolved", "polishing")) {
    pre <- x$peak_mascot_before_resolve %||% x$peak_mascot %||% "composed"
    sprintf("Mascot state: %s %s resolved", pre, arrow)
  } else if (isTRUE(x$derived_used)) {
    "Mascot state: derived escalation"
  } else {
    peak <- x$peak_mascot %||% "composed"
    sprintf("Mascot state: %s %s unresolved", peak, arrow)
  }
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
    cat(sprintf("  Outlier seeds:         %d\n",  s$outlier_count %||% 0))
    cat(sprintf("  Subgroup seeds:        %d\n",  s$subgroup_count %||% 0))
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
