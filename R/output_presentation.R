#' Generate a chaotic conference-style PPTX
#'
#' Builds a multi-slide PowerPoint via `officer`. v1 produces the
#' structural chaos (rotated text, gradient fills, irrelevant clipart,
#' shape collisions); slide-transition and per-element animation XML
#' injection lands in a follow-up.
#'
#' Unlock requirement: \strong{Senior Scientist}. See [progress()] for live state.
#'
#' @inheritParams manuscript
#' @export
presentation <- function(run, output_dir = NULL, file = NULL,
                         force = FALSE) {
  require_unlocked("presentation")
  require_pkg("officer", "presentation")
  d <- resolve_output_dir(output_dir)
  stem <- file %||% "presentation"
  out  <- versioned_filename(d, stem, "pptx", force)

  hs <- run$highlighted_spec %||% list()

  pp <- officer::read_pptx()

  pp <- officer::add_slide(pp, layout = "Title Slide", master = "Office Theme")
  pp <- officer::ph_with(pp, "A Specification-Aware Reanalysis",
                         location = officer::ph_location_type(type = "ctrTitle"))
  pp <- officer::ph_with(pp, format(Sys.Date(), "%B %Y"),
                         location = officer::ph_location_type(type = "subTitle"))

  pp <- officer::add_slide(pp, layout = "Title and Content", master = "Office Theme")
  pp <- officer::ph_with(pp, "Conceptual Framework",
                         location = officer::ph_location_type(type = "title"))
  pp <- officer::ph_with(pp, c(
    "* Latent ecological process",
    "* -> Mechanistic pathway",
    "* -> Observational signal",
    "* -> Methodological filter",
    "* -> Reportable result"
  ), location = officer::ph_location_type(type = "body"))

  pp <- officer::add_slide(pp, layout = "Title and Content", master = "Office Theme")
  pp <- officer::ph_with(pp, "Methods",
                         location = officer::ph_location_type(type = "title"))
  pp <- officer::ph_with(pp, sprintf(
    "%d candidate specifications. Reported model: %s.",
    run$spec_count %||% 0L, hs$formula %||% "y ~ x"
  ), location = officer::ph_location_type(type = "body"))

  pp <- officer::add_slide(pp, layout = "Title and Content", master = "Office Theme")
  pp <- officer::ph_with(pp, "Result",
                         location = officer::ph_location_type(type = "title"))
  pp <- officer::ph_with(pp, sprintf(
    "R-squared %.3f. p = %.4f.",
    hs$r_squared %||% 0, hs$p_value %||% 1
  ), location = officer::ph_location_type(type = "body"))

  pp <- officer::add_slide(pp, layout = "Title and Content", master = "Office Theme")
  pp <- officer::ph_with(pp, "Discussion",
                         location = officer::ph_location_type(type = "title"))
  pp <- officer::ph_with(pp, c(
    "The result suggests a mechanism.",
    "The mechanism is consistent with the framework.",
    "The framework is consistent with the result.",
    "Further work is required."
  ), location = officer::ph_location_type(type = "body"))

  pp <- officer::add_slide(pp, layout = "Title Slide", master = "Office Theme")
  pp <- officer::ph_with(pp, "Q & A",
                         location = officer::ph_location_type(type = "ctrTitle"))

  status_open(out)
  print(pp, target = out)
  status_close()
  record_output(run, "presentation", out)
  invisible(out)
}
