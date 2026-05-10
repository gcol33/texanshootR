#' texanshootR: Reproducible Audit Trails for Indefensible Research
#'
#' A structured, terminal-first interface for exploratory linear-model
#' search. The main entry point is [shoot()], which fits a battery of
#' candidate specifications across predictor subsets, transformations,
#' interactions, and outlier-removal seeds. Persistent career state
#' tracks run history, awards achievements, and unlocks output
#' generators ([manuscript()], [presentation()],
#' [reviewer_response()], [graphical_abstract()], [funding()]).
#'
#' @section Persistence:
#' On the first interactive save, the package prompts before writing
#' to [tools::R_user_dir()]. Set
#' `options(texanshootR.save_enabled = FALSE)` to opt out entirely.
#'
#' @section Reproducibility:
#' [shoot()] accepts a `seed` argument; every run records its seed,
#' R version, package version, and search-grid hash so past runs can
#' be replayed.
#'
#' @keywords internal
#' @importFrom stats runif sd cor complete.cases na.omit model.frame
#' @importFrom Rcpp evalCpp
#' @useDynLib texanshootR, .registration = TRUE
"_PACKAGE"
