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
  cat(style_header(rule), "\n", sep = "")
  cat(style_header(sprintf("Run %s", x$run_id)), "\n", sep = "")
  cat(style_header(rule), "\n", sep = "")
  cat(sprintf("Specifications fit:    %d\n", x$spec_count %||% 0L))
  hs <- x$highlighted_spec
  if (!is.null(hs)) {
    cat(sprintf("Highlighted formula:   %s\n", hs$formula %||% ""))
    cat(sprintf("Selected p-value:      %s\n",
                if (!is.null(hs$p_value)) format.pval(hs$p_value, digits = 3)
                else "-"))
    cat(sprintf("R-squared:             %s\n",
                if (!is.null(hs$r_squared)) sprintf("%.3f", hs$r_squared)
                else "-"))
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
  invisible(x)
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
