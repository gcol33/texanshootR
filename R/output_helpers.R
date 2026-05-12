# Helpers shared across all output_*.R files.

# ---- Methods-paragraph rendering ----------------------------------
#
# The search attaches structured `restriction` and `outcome_construction`
# records to each fit's result row (see R/families.R spec_metadata).
# describe_spec() turns those records plus the formula into a single
# methods-paragraph sentence that every output generator and the print
# banner share. This is the single source of truth for "what did the
# shooter actually do to get this result" -- glyph/labels stay aligned
# across the abstract, manuscript, presentation, reviewer response,
# and DATA USED block by routing through here.

render_restriction_phrase <- function(restriction) {
  if (is.null(restriction)) return(NA_character_)
  switch(restriction$kind,
    complete_cases = "restricting to complete cases",
    outcome_iqr    = "winsorising extreme observations on the outcome",
    predictor_iqr  = sprintf(
      "restricting to observations within the typical range of %s",
      restriction$var %||% "the predictor"),
    cooks_d        = "excluding observations with disproportionate leverage",
    factor_level   = sprintf("restricting to the %s subgroup of %s",
                              restriction$level %||% "selected",
                              restriction$var   %||% "the grouping variable"),
    NA_character_
  )
}

render_outcome_phrase <- function(construction, outcome) {
  if (is.null(construction)) return(NA_character_)
  switch(construction$kind,
    residualise = sprintf("partialling out variation due to %s",
                           construction$covariate %||% "the covariate"),
    ratio       = sprintf("expressing the outcome as a rate per unit of %s",
                           construction$baseline %||% "the baseline"),
    z_within    = sprintf("z-scoring the outcome within each %s",
                           construction$factor %||% "subgroup"),
    NA_character_  # composite_index handled as a lead sentence in describe_spec()
  )
}

# Model-form phrase. Renders as the model-clause tail of the methods
# sentence, e.g. "we fit a robust M-estimator of y on x". polynomial /
# spline produce predictor-wrap phrases that compose into the same
# clause; robust / family_swap / random_effect rewrite the fitter and
# produce the full model-tail.
render_model_form_phrase <- function(model_form) {
  if (is.null(model_form)) return(NA_character_)
  switch(model_form$kind,
    polynomial = sprintf("allowing for a %s trend in %s",
                          degree_word(model_form$degree %||% 2L),
                          model_form$var %||% "the predictor"),
    spline     = sprintf("modelling %s flexibly via natural splines",
                          model_form$var %||% "the predictor"),
    robust     = "using a robust M-estimator",
    family_swap = render_family_swap(model_form),
    random_effect = sprintf("with a random intercept for %s",
                              model_form$group %||% "the grouping factor"),
    NA_character_
  )
}

degree_word <- function(d) {
  switch(as.character(as.integer(d)),
    "2" = "quadratic",
    "3" = "cubic",
    sprintf("degree-%d", as.integer(d)))
}

render_family_swap <- function(mf) {
  fam_label <- switch(mf$family %||% "",
    binomial = "logistic",
    poisson  = "Poisson",
    Gamma    = "Gamma",
    mf$family %||% "GLM")
  base <- sprintf("modelled with a %s GLM", fam_label)
  coerce <- mf$outcome_coerce
  if (is.null(coerce) || !nzchar(coerce)) return(base)
  tail <- switch(coerce,
    median_dichotomize = " after dichotomising the outcome at the median",
    round_clip         = " after rounding the outcome to non-negative integers",
    "")
  paste0(base, tail)
}

# Methods-paragraph sentence for the highlighted spec. Composes the
# spec's restriction + outcome-construction + model_form records into a
# single sentence:
#   "[We constructed ...] After X and Y, we fit FORMULA, M."
# The polynomial / spline wraps are stripped from FORMULA before
# rendering so the prose clause M carries the description -- a
# methods paragraph reads "we fit weight ~ age + sex, allowing for a
# quadratic trend in age", not "we fit weight ~ poly(age, 2) + sex".
describe_spec <- function(spec) {
  if (is.null(spec)) return("We fit y ~ x.")
  formula <- strip_model_form_wraps(spec$formula %||% "y ~ x",
                                     spec$model_form)

  prefix_clauses <- character()
  lead <- NULL

  r_phrase <- render_restriction_phrase(spec$restriction)
  if (!is.na(r_phrase)) prefix_clauses <- c(prefix_clauses, r_phrase)

  oc <- spec$outcome_construction
  if (!is.null(oc)) {
    if (identical(oc$kind, "composite_index")) {
      vars <- oc$vars %||% character()
      if (length(vars)) {
        lead <- sprintf("We constructed a composite outcome from %s.",
                        oxford_join(vars))
      }
    } else {
      o_phrase <- render_outcome_phrase(oc, formula)
      if (!is.na(o_phrase)) prefix_clauses <- c(prefix_clauses, o_phrase)
    }
  }

  main <- if (length(prefix_clauses)) {
    joined <- oxford_join(prefix_clauses)
    sprintf("After %s, we fit %s", joined, formula)
  } else {
    sprintf("We fit %s", formula)
  }

  mf_phrase <- render_model_form_phrase(spec$model_form)
  main <- if (!is.na(mf_phrase)) sprintf("%s, %s.", main, mf_phrase)
          else                    paste0(main, ".")

  if (is.null(lead)) main else paste(lead, main)
}

# Rewrite poly(var, k) and splines::ns(var, df = k) terms back to bare
# `var` for the methods-paragraph rendering. The formula was built by
# materialise_spec via string concatenation -- it was never deparsed
# user code -- so a structural string substitution is safe here.
strip_model_form_wraps <- function(f, model_form) {
  if (is.null(model_form) || is.null(model_form$var)) return(f)
  var <- model_form$var
  if (identical(model_form$kind, "polynomial")) {
    pat <- sprintf("poly\\(\\Q%s\\E,\\s*\\d+\\)", var)
    return(sub(pat, var, f))
  }
  if (identical(model_form$kind, "spline")) {
    pat <- sprintf("splines::ns\\(\\Q%s\\E,\\s*df\\s*=\\s*\\d+\\)", var)
    return(sub(pat, var, f))
  }
  f
}

oxford_join <- function(x) {
  x <- as.character(x)
  if (length(x) == 0L) return("")
  if (length(x) == 1L) return(x)
  if (length(x) == 2L) return(paste(x, collapse = " and "))
  paste0(paste(x[-length(x)], collapse = ", "), ", and ", x[length(x)])
}

capitalize_first <- function(s) {
  if (!nzchar(s)) return(s)
  paste0(toupper(substr(s, 1L, 1L)), substr(s, 2L, nchar(s)))
}

# Compact methods summary for the print banner FINDING line. Returns a
# semicolon-joined list of perturbation phrases, or "none" when no
# restriction / outcome construction was applied. Distinct from
# describe_spec(): no leading capital, no "we fit FORMULA" tail.
methods_summary <- function(spec) {
  if (is.null(spec)) return("none")
  parts <- character()
  r <- render_restriction_phrase(spec$restriction)
  if (!is.na(r)) parts <- c(parts, r)
  oc <- spec$outcome_construction
  if (!is.null(oc)) {
    if (identical(oc$kind, "composite_index")) {
      vars <- oc$vars %||% character()
      if (length(vars)) {
        parts <- c(parts,
                   sprintf("composite outcome from %s", oxford_join(vars)))
      }
    } else {
      o <- render_outcome_phrase(oc, "")
      if (!is.na(o)) parts <- c(parts, o)
    }
  }
  mf <- render_model_form_phrase(spec$model_form)
  if (!is.na(mf)) parts <- c(parts, mf)
  if (!length(parts)) return("none")
  paste(parts, collapse = "; ")
}

# ---- File-output helpers ------------------------------------------

resolve_output_dir <- function(output_dir = NULL) {
  d <- output_dir %||% getOption("texanshootR.output_dir") %||% tempdir()
  if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
  d
}

require_pkg <- function(pkg, fn) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(sprintf("`%s()` requires the '%s' package. Install it with install.packages('%s').",
                 fn, pkg, pkg), call. = FALSE)
  }
  invisible(TRUE)
}

# Filename versioning rotor - if a default-named file already exists
# we step through ("file.ext", "file_v2.ext", "file_v2_REAL.ext",
# "file_v2_REAL_FINAL.ext", "file_v2_REAL_FINAL_clean.ext").
SUFFIX_LADDER <- c("", "_v2", "_v2_REAL", "_v2_REAL_FINAL", "_v2_REAL_FINAL_clean")

versioned_filename <- function(dir, stem, ext, force = FALSE) {
  for (sfx in SUFFIX_LADDER) {
    f <- file.path(dir, paste0(stem, sfx, ".", ext))
    if (!file.exists(f) || isTRUE(force)) return(f)
  }
  # Final fallback: timestamp.
  file.path(dir, sprintf("%s_%s.%s", stem,
                         format(Sys.time(), "%Y%m%d%H%M%S"), ext))
}

status_open  <- function(file) {
  if (isTRUE(opt("texanshootR.quiet"))) return(invisible())
  cat("Saving ", basename(file), "...\n", sep = "")
}
status_close <- function() {
  if (isTRUE(opt("texanshootR.quiet"))) return(invisible())
  cat("Done.\n")
}

# Track that a run produced a given output. Updates the per-run record
# on disk and re-evaluates achievements so output-driven triggers (HARK,
# all-outputs, filename archaeologist, publication pipeline) can fire.
record_output <- function(run, output_name, file) {
  d <- save_dir()
  if (is.null(d)) return(invisible())
  fpath <- file.path(d, "runs", paste0(run$run_id, ".rds"))
  if (!file.exists(fpath)) return(invisible())
  rec <- readRDS(fpath)
  rec$outputs_generated       <- unique(c(rec$outputs_generated %||% character(), output_name))
  rec$outputs_generated_files <- unique(c(rec$outputs_generated_files %||% character(), basename(file)))

  # Carry over runtime-set flags from the in-memory run (e.g. `harked`
  # is set by manuscript() before this call). The on-disk record is
  # what evaluate_achievements() reads.
  for (k in c("harked", "peak_mascot",
              "stopped_early", "resolved_at_progress", "ultra_rare_seen")) {
    v <- run[[k]]
    if (!is.null(v)) rec[[k]] <- v
  }
  saveRDS(rec, fpath)

  meta <- read_meta()
  if (!is.null(meta)) {
    meta$hidden$output_complexity <- (meta$hidden$output_complexity %||% 0) +
      output_complexity_score(output_name)
    write_meta(meta)
    award_and_equip(rec, meta)
  }
  invisible()
}

output_complexity_score <- function(name) {
  switch(name,
    manuscript         = 3,
    presentation       = 2,
    reviewer_response  = 1,
    graphical_abstract = 1,
    funding            = 2,
    1
  )
}

# Map between the CHAIN_STAGES token and the exported generator. Kept
# here so generate_flagged_outputs() doesn't have to know the function
# names directly and so a future stage rename touches one table.
OUTPUT_GENERATORS <- list(
  abstract           = function(run) abstract(run),
  manuscript         = function(run) manuscript(run),
  presentation       = function(run) presentation(run),
  reviewer_response  = function(run) reviewer_response(run),
  graphical_abstract = function(run) graphical_abstract(run),
  funding            = function(run) funding(run)
)

# Auto-generate the chain prefix needed to satisfy the caller's output
# flags. Walks CHAIN_STAGES in order and calls each generator up to the
# highest enabled flag. A locked stage (xp_threshold not yet met) or an
# expired chain window stops the walk -- the generator throws a
# tx_chain_error which we catch and surface as a one-line note. A
# non-shippable run has no active chain, so the first call short-circuits.
generate_flagged_outputs <- function(run, flags) {
  if (!length(flags)) return(run)
  enabled <- vapply(flags, isTRUE, logical(1))
  if (!any(enabled)) return(run)
  if (!isTRUE(run$shippable)) return(run)
  highest <- max(stage_index(names(flags)[enabled]), na.rm = TRUE)
  for (i in seq_len(highest)) {
    stage <- CHAIN_STAGES[[i]]
    gen   <- OUTPUT_GENERATORS[[stage]]
    if (is.null(gen)) next
    res <- tryCatch(gen(run),
                    tx_chain_error = function(e) e,
                    error          = function(e) e)
    if (inherits(res, "tx_chain_error")) {
      say(sprintf("Skipping %s and later stages: %s.", stage, res$reason))
      break
    }
  }
  # Merge the on-disk record's outputs_generated fields back into the
  # in-memory run -- record_output() updates the rds but doesn't touch
  # the local `run` object, so without this re-read shoot() would
  # return a run with empty outputs_generated.
  d <- save_dir()
  if (!is.null(d)) {
    fpath <- file.path(d, "runs", paste0(run$run_id, ".rds"))
    if (file.exists(fpath)) {
      updated <- readRDS(fpath)
      for (k in c("outputs_generated", "outputs_generated_files")) {
        if (!is.null(updated[[k]])) run[[k]] <- updated[[k]]
      }
    }
  }
  run
}
