#' Generate a funding letter + budget DOCX
#'
#' One-page Letter of Intent with a deadpan budget table.
#'
#' Unlock requirement: \strong{PI}. See [progress()] for live state.
#'
#' @inheritParams manuscript
#' @export
funding <- function(run, output_dir = NULL, file = NULL, force = FALSE) {
  require_unlocked("funding")
  require_pkg("officer", "funding")
  d <- resolve_output_dir(output_dir)
  stem <- file %||% "funding_proposal_clean"
  out  <- versioned_filename(d, stem, "docx", force)

  hs <- run$highlighted_spec %||% list()

  doc <- officer::read_docx()
  doc <- officer::body_add_par(doc, "Letter of Intent", style = "heading 1")
  doc <- officer::body_add_par(doc, sprintf(
    "We propose a research programme building directly on the relationship %s, ",
    hs$formula %||% "y ~ x"
  ))
  doc <- officer::body_add_par(doc, paste0(
    "which our preliminary analysis has identified with R-squared ",
    sprintf("%.3f", hs$r_squared %||% 0),
    " (p = ",
    sprintf("%.4f", hs$p_value %||% 1),
    "). The proposed work will scale this finding across additional ",
    "ecological contexts with broad policy relevance."))

  doc <- officer::body_add_par(doc, "")
  doc <- officer::body_add_par(doc, "Budget", style = "heading 2")

  budget <- data.frame(
    Item       = c("Personnel: 1 unspecified postdoc",
                   "Personnel: 0.2 FTE PI",
                   "Compute and storage",
                   "Open-access publication fees",
                   "Conference travel",
                   "Indirect costs"),
    EUR        = c(72000, 18000, 4000, 6000, 5000, 32500),
    stringsAsFactors = FALSE
  )

  doc <- officer::body_add_table(doc, value = budget,
                                 style = "table_template")

  doc <- officer::body_add_par(doc, "")
  doc <- officer::body_add_par(doc, "Total: 137 500 EUR.")

  status_open(out)
  print(doc, target = out)
  status_close()
  record_output(run, "funding", out)
  invisible(out)
}
