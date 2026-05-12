#' Generate a response-to-reviewers DOCX
#'
#' Polite, point-by-point response to imagined reviewer comments.
#'
#' Chain stage: \strong{reviewer_response} (length 4). See [progress()].
#'
#' @inheritParams manuscript
#' @export
reviewer_response <- function(run, output_dir = NULL, file = NULL,
                              force = FALSE) {
  require_chain_stage("reviewer_response", run)
  require_pkg("officer", "reviewer_response")

  # Roll the reviewer outcome the first time the player asks for a
  # response. Subsequent calls reuse the persisted verdict. The
  # `received_a_reject` achievement (and any future outcome-driven
  # trigger) fires via record_output() below, which re-reads the
  # updated record and re-runs evaluate_achievements().
  run <- materialize_reviewer_outcome(run)

  d <- resolve_output_dir(output_dir)
  stem <- file %||% "response_to_reviewers_FINAL"
  out  <- versioned_filename(d, stem, "docx", force)

  hs <- run$highlighted_spec %||% list()
  methods <- describe_spec(hs)

  doc <- officer::read_docx()
  doc <- officer::body_add_par(doc, "Response to Reviewers", style = "heading 1")
  doc <- officer::body_add_par(doc, "We thank the reviewers for their thorough engagement with the manuscript.")
  doc <- officer::body_add_par(doc, paste0(
    "For reference, the analysis presented in the manuscript is summarised as: ",
    methods))
  doc <- officer::body_add_par(doc, "")

  comments <- list(
    list(reviewer = "Reviewer 1",
         comment  = "The conceptual framework should be tightened.",
         response = "We have tightened the conceptual framework."),
    list(reviewer = "Reviewer 2",
         comment  = "Additional analyses are warranted.",
         response = "Additional analyses, consistent with our prior framing, have been added."),
    list(reviewer = "Reviewer 1",
         comment  = "Clarify the choice of transformation.",
         response = "The chosen transformation is consistent with the literature."),
    list(reviewer = "Reviewer 2",
         comment  = "Sample size limits inference.",
         response = "We have noted this limitation. The reported specification remains robust within the analysed subset.")
  )

  for (c in comments) {
    doc <- officer::body_add_par(doc, c$reviewer, style = "heading 2")
    doc <- officer::body_add_par(doc, paste("Comment:", c$comment))
    doc <- officer::body_add_par(doc, paste("Response:", c$response))
    doc <- officer::body_add_par(doc, "")
  }

  status_open(out)
  print(doc, target = out)
  status_close()
  record_output(run, "reviewer_response", out)
  advance_chain_after_stage("reviewer_response")
  invisible(out)
}
