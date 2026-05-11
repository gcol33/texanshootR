#' Generate a one-paragraph deadpan abstract
#'
#' The entry-tier output. Always the first stage of the publication
#' chain (see [progress()]). Writes a plain-text `.txt` file describing
#' the highlighted specification in the unbothered register typical of
#' an applied-stats abstract.
#'
#' Chain stage: \strong{abstract} (length 1, always unlocked).
#'
#' @param run A `tx_run` object returned by [shoot()]. Must be the
#'   currently active publication chain — i.e., the run that opened
#'   the chain when it cleared `p <= 0.05`.
#' @param output_dir Optional output directory.
#' @param file Optional filename stem (without extension).
#' @param force Overwrite an existing file.
#' @return Character path to the written file (invisible).
#' @export
abstract <- function(run, output_dir = NULL, file = NULL,
                     force = FALSE) {
  require_chain_stage("abstract", run)
  d   <- resolve_output_dir(output_dir)
  stem <- file %||% "abstract"
  out  <- versioned_filename(d, stem, "txt", force)

  text <- build_abstract_text(run)
  status_open(out)
  writeLines(text, con = out)
  status_close()
  record_output(run, "abstract", out)
  advance_chain_after_stage("abstract")
  invisible(out)
}

build_abstract_text <- function(run) {
  hs <- run$highlighted_spec %||% list()
  formula  <- hs$formula  %||% "y ~ x"
  rsq      <- hs$r_squared %||% 0
  pval     <- hs$p_value  %||% 1
  n_specs  <- run$spec_count %||% 0L

  paste(
    "Abstract",
    "",
    paste0(
      "We examined the relationship described by ",
      formula, ". A targeted specification search across ",
      n_specs, " candidate models identified a configuration ",
      "consistent with prior expectations. The reported model ",
      sprintf("recovered an R-squared of %.3f with a p-value of %.4f. ",
              rsq, pval),
      "These results inform a coherent picture of the underlying ",
      "process and warrant further investigation."
    ),
    sep = "\n"
  )
}
