#' Generate a preprint HTML
#'
#' bioRxiv-style HTML rendering of a run's highlighted specification.
#' Includes a fake DOI and a deadpan "Comments (0)" footer.
#'
#' Unlock requirement: \strong{Postdoc}. See [progress()] for live state.
#'
#' @inheritParams manuscript
#' @export
preprint <- function(run, output_dir = NULL, file = NULL, force = FALSE) {
  require_unlocked("preprint")
  require_pkg("rmarkdown", "preprint")
  d <- resolve_output_dir(output_dir)
  stem <- file %||% "preprint_v2"
  out  <- versioned_filename(d, stem, "html", force)

  rmd_text <- build_preprint_rmd(run)
  rmd_path <- tempfile(fileext = ".Rmd")
  writeLines(rmd_text, rmd_path)

  status_open(out)
  rmarkdown::render(rmd_path, output_format = rmarkdown::html_document(),
                    output_file = basename(out),
                    output_dir  = dirname(out), quiet = TRUE)
  status_close()

  record_output(run, "preprint", out)
  invisible(out)
}

build_preprint_rmd <- function(run) {
  hs <- run$highlighted_spec %||% list()
  doi <- sprintf("10.0000/preprint.%s",
                 substr(gsub("[^0-9]", "", run$run_id %||% "00000000"), 1, 10))
  paste(c(
    "---",
    "title: 'A Specification-Aware Reanalysis'",
    "output: html_document",
    "---",
    "",
    sprintf("**DOI:** %s", doi),
    "",
    "**Status:** Posted (not yet peer-reviewed)",
    "",
    "## Abstract",
    "",
    "We re-examine the dataset with a specification chosen for ",
    "interpretability.",
    "",
    "## Result",
    "",
    sprintf("Highlighted specification: `%s`. R-squared %.3f. p = %.4f.",
            hs$formula %||% "y ~ x",
            hs$r_squared %||% 0,
            hs$p_value %||% 1),
    "",
    "---",
    "",
    "*Comments (0)*"
  ), collapse = "\n")
}
