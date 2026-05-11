#' Generate a manuscript from a run
#'
#' Renders an academic-style write-up of the highlighted specification.
#' If `quarto` and `tinytex` are available, produces a PDF + DOCX; if
#' only `rmarkdown` is available, produces a DOCX. The package never
#' installs LaTeX on its own.
#'
#' Chain stage: \strong{manuscript} (length 2). See [progress()].
#'
#' @param run A `tx_run` object returned by [shoot()].
#' @param output_dir Optional output directory.
#' @param file Optional filename stem (without extension).
#' @param force Overwrite an existing file.
#' @return Character vector of file paths (invisible).
#' @export
manuscript <- function(run, output_dir = NULL, file = NULL,
                       force = FALSE) {
  require_chain_stage("manuscript", run)
  require_pkg("rmarkdown", "manuscript")
  d <- resolve_output_dir(output_dir)
  stem <- file %||% "manuscript_clean"

  rmd_text <- build_manuscript_rmd(run)
  rmd_path <- tempfile(fileext = ".Rmd")
  writeLines(rmd_text, rmd_path)

  out_files <- character()

  docx_path <- versioned_filename(d, stem, "docx", force)
  status_open(docx_path)
  rmarkdown::render(rmd_path, output_format = rmarkdown::word_document(),
                    output_file = basename(docx_path),
                    output_dir  = dirname(docx_path), quiet = TRUE)
  status_close()
  out_files <- c(out_files, docx_path)

  if (requireNamespace("quarto", quietly = TRUE) &&
      requireNamespace("tinytex", quietly = TRUE) &&
      tinytex::is_tinytex()) {
    pdf_path <- versioned_filename(d, stem, "pdf", force)
    status_open(pdf_path)
    rmarkdown::render(rmd_path, output_format = rmarkdown::pdf_document(),
                      output_file = basename(pdf_path),
                      output_dir  = dirname(pdf_path), quiet = TRUE)
    status_close()
    out_files <- c(out_files, pdf_path)
  }

  run$harked <- TRUE
  record_output(run, "manuscript", out_files[1])
  advance_chain_after_stage("manuscript")
  invisible(out_files)
}

build_manuscript_rmd <- function(run) {
  hs <- run$highlighted_spec %||% list()
  paste(c(
    "---",
    "title: 'A Reanalysis with Improved Specification'",
    "author: 'Anonymous'",
    "output: word_document",
    "---",
    "",
    "## Abstract",
    "",
    "We revisit the dataset with attention to specification choice ",
    "and report the relationship that emerges from a specification ",
    "consistent with prior expectations.",
    "",
    "## Methods",
    "",
    sprintf("We considered %d candidate specifications across predictor ",
            run$spec_count %||% 0L),
    "subsets, transformations, interaction terms, and observation ",
    "subsets. The reported specification was selected for its ",
    "consistency with the conceptual framework.",
    "",
    "## Results",
    "",
    sprintf("The final specification was `%s` with R-squared %.3f and ",
            hs$formula %||% "y ~ x",
            hs$r_squared %||% 0),
    sprintf("p = %.4f.", hs$p_value %||% 1),
    "",
    "## Discussion",
    "",
    "The result suggests a pathway consistent with prior ",
    "ecological reasoning. We controlled for what was available."
  ), collapse = "\n")
}
