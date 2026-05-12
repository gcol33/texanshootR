# Stateful, opportunistic spec generator.
#
# The shooter does not enumerate a clean cartesian product. He starts
# from a small seed pool (one spec per predictor subset, identity
# transforms, no perturbations) and then drifts toward whatever subset
# is producing the most encouraging p-values. Subsets that go cold
# get abandoned. Near-misses (best_p in (0.05, 0.20]) get re-perturbed
# along the other axes. A wildcard is injected periodically to keep
# him moving when nothing is pulsing.
#
# This is desperation, not exploration. Each fork in the search is the
# shooter trying one more thing because the last thing was almost.
#
# Mechanic-to-fallacy contract:
#   subsets       = predictor dredging / multiple comparisons
#   transforms    = researcher degrees of freedom
#   interactions  = p-hacking
#   restrictions  = principled-sounding sample exclusion
#                   (complete cases, IQR fence, Cook's D, factor restriction)
#   outcome_con   = outcome engineering
#                   (composites, residualisation, ratios, within-group z)
#   model_form    = model-complexity escalation
#                   (polynomial / spline wraps, robust M-estimator,
#                   GLM family swap, random-intercept lift)

NEAR_MISS_RANGE       <- c(0.05, 0.20)
DEAD_AFTER_TRIES      <- 5L
DEAD_P_THRESHOLD      <- 0.5
WILDCARD_EVERY        <- 10L
PERTURBATIONS_PER_HIT <- 3L
PERTURB_KINDS         <- c("transform", "interaction", "restriction",
                            "outcome", "model")

new_spec <- function(subset,
                     transforms           = NULL,
                     interactions         = character(),
                     restriction          = NULL,
                     outcome_construction = NULL,
                     model_form           = NULL,
                     source               = "seed",
                     family               = list(fitter = "lm")) {
  if (is.null(transforms)) {
    transforms <- stats::setNames(rep("identity", length(subset)), subset)
  }
  list(subset               = subset,
       transforms           = transforms,
       interactions         = interactions,
       restriction          = restriction,
       outcome_construction = outcome_construction,
       model_form           = model_form,
       source               = source,
       family               = family)
}

subset_key <- function(spec) paste(sort(spec$subset), collapse = ",")

# Initial pool: one identity-only spec per non-empty subset of size 1..k,
# in random order so the shooter doesn't grind methodically.
seed_specs <- function(df, outcome, predictors, max_subset_size = 4L) {
  predictors <- intersect(predictors, names(df))
  predictors <- setdiff(predictors, outcome)
  if (length(predictors) == 0L) return(list())
  ks <- seq_len(min(max_subset_size, length(predictors)))
  specs <- list()
  for (k in ks) {
    if (length(predictors) >= k) {
      combs <- utils::combn(predictors, k, simplify = FALSE)
      for (s in combs) {
        specs[[length(specs) + 1L]] <- new_spec(s, source = "seed")
      }
    }
  }
  specs[sample.int(length(specs))]
}

# Perturb a single dimension of an existing spec. Used both for
# near-miss follow-ups and for assembling wildcards.
perturb_spec <- function(spec, kind, df, outcome) {
  s <- spec
  s$source <- paste0("perturb_", kind)
  if (kind == "transform") {
    nums <- intersect(s$subset, names(df))
    nums <- nums[vapply(nums, function(p) is.numeric(df[[p]]), logical(1))]
    if (length(nums)) {
      var <- if (length(nums) == 1L) nums else sample(nums, 1L)
      s$transforms[[var]] <- sample(c("log", "sqrt", "scale"), 1L)
    }
  } else if (kind == "interaction" && length(s$subset) >= 2L) {
    pair <- sample(s$subset, 2L)
    s$interactions <- unique(c(s$interactions,
                               paste(sort(pair), collapse = ":")))
  } else if (kind == "restriction") {
    pick <- sample_restriction(df, outcome, s)
    if (!is.null(pick)) s$restriction <- pick
  } else if (kind == "outcome") {
    pick <- sample_outcome_construction(df, outcome, s)
    if (!is.null(pick)) s$outcome_construction <- pick
  } else if (kind == "model") {
    pick <- sample_model_form(df, outcome, s)
    if (!is.null(pick)) s$model_form <- pick
  }
  s
}

# Random subset + 1-3 random perturbations. Injected when the search
# stalls or periodically to keep things flailing.
wildcard_spec <- function(df, outcome, predictors, max_subset_size = 4L) {
  predictors <- intersect(predictors, names(df))
  predictors <- setdiff(predictors, outcome)
  if (length(predictors) == 0L) return(NULL)
  k_max <- min(max_subset_size, length(predictors))
  k <- if (k_max == 1L) 1L else sample.int(k_max, 1L)
  subset <- if (length(predictors) == k) predictors
            else sample(predictors, k)
  s <- new_spec(subset, source = "wildcard")
  kinds <- sample(PERTURB_KINDS, size = sample.int(3L, 1L))
  for (kk in kinds) s <- perturb_spec(s, kk, df, outcome)
  s$source <- "wildcard"
  s
}

new_search_state <- function(seeds) {
  list(
    seed_queue     = seeds,
    perturb_queue  = list(),
    dead_subsets   = character(),
    subset_results = list()
  )
}

# Update state with the just-completed fit: append p-value to the
# subset's history; if the subset is in the near-miss band, enqueue a
# few perturbations of THIS spec; if the subset has gone stone cold,
# abandon it.
#
# `bias_perturb` (NA_character_ by default) is the kind to over-sample
# when the player has injected a perturbation bias (+subgroup, +outliers).
# When set and valid, ~2/3 of the enqueued perturbations are that kind;
# the rest stay random so the search doesn't collapse onto one axis.
record_result <- function(state, spec, result, df, outcome,
                          bias_perturb = NA_character_) {
  if (is.null(result)) return(state)
  key <- subset_key(spec)
  ps  <- c(state$subset_results[[key]] %||% numeric(),
           result$p_value %||% NA_real_)
  state$subset_results[[key]] <- ps

  best_p <- suppressWarnings(min(ps, na.rm = TRUE))
  if (!is.finite(best_p)) best_p <- NA_real_

  if (is.finite(best_p) &&
      best_p > NEAR_MISS_RANGE[1] && best_p <= NEAR_MISS_RANGE[2]) {
    kinds <- biased_perturb_kinds(bias_perturb, PERTURBATIONS_PER_HIT)
    for (kk in kinds) {
      state$perturb_queue[[length(state$perturb_queue) + 1L]] <-
        perturb_spec(spec, kk, df, outcome)
    }
  }

  if (length(ps) >= DEAD_AFTER_TRIES &&
      all(is.na(ps) | ps > DEAD_P_THRESHOLD)) {
    state$dead_subsets <- unique(c(state$dead_subsets, key))
  }
  state
}

# Sample N perturbation kinds, optionally over-sampling a biased tag.
# Without a bias the result is a plain uniform draw -- matches the
# previous behaviour exactly. With a bias, 2/3 of slots are forced to
# the biased kind and 1/3 fall back to uniform random.
biased_perturb_kinds <- function(bias_tag, n) {
  if (is.null(bias_tag) || is.na(bias_tag) || !nzchar(bias_tag) ||
      !(bias_tag %in% PERTURB_KINDS)) {
    return(sample(PERTURB_KINDS, size = n))
  }
  rolls <- stats::runif(n)
  ifelse(rolls < 2/3, bias_tag,
         sample(PERTURB_KINDS, size = n, replace = TRUE))
}

# Decide what to fit next. Priority: scheduled wildcard tick > pending
# perturbation > next live seed. Falls back to a wildcard when seeds
# are exhausted, so the loop can run to budget instead of stopping
# politely.
next_spec <- function(state, df, outcome, predictors,
                      iteration, max_subset_size = 4L) {
  if (iteration > 0L && iteration %% WILDCARD_EVERY == 0L) {
    return(list(state = state,
                spec  = wildcard_spec(df, outcome, predictors,
                                      max_subset_size)))
  }
  if (length(state$perturb_queue)) {
    spec <- state$perturb_queue[[1L]]
    state$perturb_queue <- state$perturb_queue[-1L]
    return(list(state = state, spec = spec))
  }
  while (length(state$seed_queue)) {
    spec <- state$seed_queue[[1L]]
    state$seed_queue <- state$seed_queue[-1L]
    if (!(subset_key(spec) %in% state$dead_subsets)) {
      return(list(state = state, spec = spec))
    }
  }
  list(state = state,
       spec  = wildcard_spec(df, outcome, predictors, max_subset_size))
}

# Apply transformations + restriction + outcome construction to a data
# frame and return the modified copy plus the formula string for the
# spec.
materialise_spec <- function(df, outcome, spec) {
  d <- df

  for (var in names(spec$transforms)) {
    t <- spec$transforms[[var]]
    if (t == "identity") next
    x <- d[[var]]
    if (!is.numeric(x)) next
    d[[var]] <- switch(t,
      log   = ifelse(x > 0, log(x), NA_real_),
      sqrt  = ifelse(x >= 0, sqrt(x), NA_real_),
      scale = as.numeric(scale(x)),
      x
    )
  }

  d <- apply_restriction(d, outcome, spec$restriction, spec)
  d <- apply_outcome_construction(d, outcome, spec$outcome_construction)

  # Predictor wraps: polynomial / spline rewrite a single bare term in
  # the RHS from `x` to `poly(x, k)` / `splines::ns(x, df = k)`. Both
  # are formula-level only -- the data column is untouched, so the
  # existing model.frame / model.matrix path materialises the basis at
  # fit time. Other model_form sub-kinds (robust, family_swap,
  # random_effect) are fitter-level and don't touch the RHS here.
  predictor_terms <- spec$subset
  mf <- spec$model_form
  if (!is.null(mf) && mf$kind %in% c("polynomial", "spline") &&
      !is.null(mf$var) && mf$var %in% predictor_terms) {
    idx <- which(predictor_terms == mf$var)
    predictor_terms[idx] <- if (mf$kind == "polynomial") {
      sprintf("poly(%s, %d)", mf$var, mf$degree %||% 2L)
    } else {
      sprintf("splines::ns(%s, df = %d)", mf$var, mf$df %||% 3L)
    }
  }

  rhs <- if (length(predictor_terms) == 0L) "1"
         else paste(predictor_terms, collapse = " + ")
  if (any(nzchar(spec$interactions))) {
    its <- spec$interactions[nzchar(spec$interactions)]
    rhs <- paste(rhs, paste(its, collapse = " + "), sep = " + ")
  }
  formula_str <- paste(outcome, "~", rhs)

  list(df = d, formula = formula_str)
}

# ---- restriction sampling + application ----------------------------

# Build the candidate list of restrictions whose triggers fire on this
# df + spec, then pick one uniformly. Returns NULL when no candidate
# qualifies (e.g. df has no factor columns and the outcome is character).
sample_restriction <- function(df, outcome, spec) {
  candidates <- list(list(kind = "complete_cases"))

  n <- nrow(df)
  y <- if (outcome %in% names(df)) df[[outcome]] else NULL

  if (!is.null(y) && is.numeric(y) && n >= 30L) {
    candidates[[length(candidates) + 1L]] <- list(kind = "outcome_iqr")
    candidates[[length(candidates) + 1L]] <- list(kind = "cooks_d")
  }

  nums_in_subset <- intersect(spec$subset, names(df))
  nums_in_subset <- nums_in_subset[vapply(nums_in_subset,
                                          function(p) is.numeric(df[[p]]),
                                          logical(1))]
  if (length(nums_in_subset)) {
    var <- if (length(nums_in_subset) == 1L) nums_in_subset
           else sample(nums_in_subset, 1L)
    candidates[[length(candidates) + 1L]] <- list(kind = "predictor_iqr",
                                                  var  = var)
  }

  factors <- categorical_columns_outside_subset(df, outcome, spec$subset)
  for (fac in factors) {
    levels_pool <- viable_factor_levels(df[[fac]], min_per_level = 10L)
    if (length(levels_pool)) {
      lvl <- if (length(levels_pool) == 1L) levels_pool
             else sample(levels_pool, 1L)
      candidates[[length(candidates) + 1L]] <- list(kind  = "factor_level",
                                                    var   = fac,
                                                    level = lvl)
      break  # one factor-level candidate per perturbation
    }
  }

  candidates[[sample.int(length(candidates), 1L)]]
}

apply_restriction <- function(d, outcome, restriction, spec) {
  if (is.null(restriction)) return(d)
  kind <- restriction$kind

  if (kind == "complete_cases") {
    needed <- intersect(unique(c(outcome, spec$subset)), names(d))
    if (!length(needed)) return(d)
    keep <- stats::complete.cases(d[, needed, drop = FALSE])
    return(d[keep, , drop = FALSE])
  }

  if (kind == "outcome_iqr") {
    if (!(outcome %in% names(d))) return(d)
    y <- d[[outcome]]
    if (!is.numeric(y)) return(d)
    return(d[iqr_keep(y), , drop = FALSE])
  }

  if (kind == "predictor_iqr") {
    v <- restriction$var
    if (!(v %in% names(d)) || !is.numeric(d[[v]])) return(d)
    return(d[iqr_keep(d[[v]]), , drop = FALSE])
  }

  if (kind == "cooks_d") {
    if (!(outcome %in% names(d))) return(d)
    if (nrow(d) < 30L) return(d)
    needed <- intersect(c(outcome, spec$subset), names(d))
    d_clean <- d[stats::complete.cases(d[, needed, drop = FALSE]), ,
                 drop = FALSE]
    if (nrow(d_clean) < 30L) return(d)
    rhs <- if (length(spec$subset)) paste(spec$subset, collapse = " + ")
           else "1"
    f <- tryCatch(stats::as.formula(paste(outcome, "~", rhs)),
                  error = function(e) NULL)
    if (is.null(f)) return(d)
    fit <- tryCatch(stats::lm(f, data = d_clean), error = function(e) NULL)
    if (is.null(fit)) return(d)
    cd <- tryCatch(stats::cooks.distance(fit), error = function(e) NULL)
    if (is.null(cd) || !any(is.finite(cd))) return(d)
    drop_n <- max(1L, as.integer(0.05 * length(cd)))
    drop_rows <- order(cd, decreasing = TRUE)[seq_len(drop_n)]
    keep_rows <- setdiff(seq_len(nrow(d_clean)), drop_rows)
    return(d_clean[keep_rows, , drop = FALSE])
  }

  if (kind == "factor_level") {
    v <- restriction$var
    lvl <- restriction$level
    if (!(v %in% names(d))) return(d)
    keep <- !is.na(d[[v]]) & as.character(d[[v]]) == lvl
    return(d[keep, , drop = FALSE])
  }

  d
}

iqr_keep <- function(x) {
  qs <- stats::quantile(x, c(0.25, 0.75), na.rm = TRUE)
  iqr <- qs[2] - qs[1]
  lo <- qs[1] - 1.5 * iqr
  hi <- qs[2] + 1.5 * iqr
  !is.na(x) & x >= lo & x <= hi
}

# ---- outcome-construction sampling + application -------------------

# Build the candidate list of outcome-engineering moves whose triggers
# fire on this df + spec, then pick one uniformly. Returns NULL when
# nothing qualifies (e.g. df has no other numeric columns to compose).
sample_outcome_construction <- function(df, outcome, spec) {
  candidates <- list()
  n <- nrow(df)
  y <- if (outcome %in% names(df)) df[[outcome]] else NULL

  numeric_cols <- names(df)[vapply(names(df),
                                    function(nm) is.numeric(df[[nm]]),
                                    logical(1))]
  outside <- setdiff(numeric_cols, c(outcome, spec$subset))

  if (length(outside) >= 2L) {
    k_max <- min(4L, length(outside))
    k <- if (k_max == 2L) 2L else sample(2L:k_max, 1L)
    picked <- if (length(outside) == k) outside else sample(outside, k)
    candidates[[length(candidates) + 1L]] <- list(kind = "composite_index",
                                                  vars = picked)
  }

  if (length(outside) >= 1L) {
    cov <- if (length(outside) == 1L) outside else sample(outside, 1L)
    candidates[[length(candidates) + 1L]] <- list(kind      = "residualise",
                                                  covariate = cov)
  }

  positive_outside <- outside[vapply(outside, function(v) {
    x <- df[[v]]
    finite <- is.finite(x)
    any(finite) && all(x[finite] > 0)
  }, logical(1))]
  if (length(positive_outside) && !is.null(y) && is.numeric(y)) {
    base <- if (length(positive_outside) == 1L) positive_outside
            else sample(positive_outside, 1L)
    candidates[[length(candidates) + 1L]] <- list(kind     = "ratio",
                                                  baseline = base)
  }

  factors <- categorical_columns_outside_subset(df, outcome, spec$subset)
  if (length(factors) && !is.null(y) && is.numeric(y)) {
    fac <- if (length(factors) == 1L) factors else sample(factors, 1L)
    candidates[[length(candidates) + 1L]] <- list(kind   = "z_within",
                                                  factor = fac)
  }

  if (!length(candidates)) return(NULL)
  candidates[[sample.int(length(candidates), 1L)]]
}

apply_outcome_construction <- function(d, outcome, construction) {
  if (is.null(construction)) return(d)
  kind <- construction$kind

  if (kind == "composite_index") {
    vars <- intersect(construction$vars, names(d))
    if (length(vars) < 2L) return(d)
    M <- as.matrix(d[, vars, drop = FALSE])
    M_scaled <- scale(M)
    new_y <- rowMeans(M_scaled, na.rm = TRUE)
    new_y[is.nan(new_y)] <- NA_real_
    d[[outcome]] <- new_y
    return(d)
  }

  if (kind == "residualise") {
    cov <- construction$covariate
    if (!(cov %in% names(d)) || !(outcome %in% names(d))) return(d)
    y <- d[[outcome]]
    z <- d[[cov]]
    if (!is.numeric(y) || !is.numeric(z)) return(d)
    ok <- is.finite(y) & is.finite(z)
    if (sum(ok) < 5L) return(d)
    fit <- tryCatch(stats::lm(y[ok] ~ z[ok]), error = function(e) NULL)
    if (is.null(fit)) return(d)
    resids <- rep(NA_real_, length(y))
    resids[ok] <- stats::residuals(fit)
    d[[outcome]] <- resids
    return(d)
  }

  if (kind == "ratio") {
    base <- construction$baseline
    if (!(base %in% names(d)) || !is.numeric(d[[base]])) return(d)
    if (!(outcome %in% names(d)) || !is.numeric(d[[outcome]])) return(d)
    b <- d[[base]]
    y <- d[[outcome]]
    ok <- is.finite(b) & b > 0
    y[!ok] <- NA_real_
    y[ok]  <- y[ok] / b[ok]
    d[[outcome]] <- y
    return(d)
  }

  if (kind == "z_within") {
    fac <- construction$factor
    if (!(fac %in% names(d)) || !(outcome %in% names(d))) return(d)
    y <- d[[outcome]]
    if (!is.numeric(y)) return(d)
    f <- as.character(d[[fac]])
    new_y <- y
    for (lvl in unique(f[!is.na(f)])) {
      idx <- which(f == lvl)
      if (length(idx) < 3L) next
      mu <- mean(y[idx], na.rm = TRUE)
      sdv <- stats::sd(y[idx], na.rm = TRUE)
      if (!is.finite(sdv) || sdv == 0) next
      new_y[idx] <- (y[idx] - mu) / sdv
    }
    d[[outcome]] <- new_y
    return(d)
  }

  d
}

# ---- shared helpers -----------------------------------------------

categorical_columns_outside_subset <- function(df, outcome, subset) {
  cols <- setdiff(names(df), c(outcome, subset))
  cols[vapply(cols, function(cn) {
    x <- df[[cn]]
    is.factor(x) ||
      (is.character(x) && length(unique(x[!is.na(x)])) >= 2L)
  }, logical(1))]
}

viable_factor_levels <- function(x, min_per_level = 10L) {
  if (is.character(x)) x <- factor(x)
  tab <- table(x)
  names(tab)[tab >= min_per_level]
}

# Compact digest for one spec restriction/outcome_construction record.
# Used by digest_grid()/summarise_trace() in shoot.R to detect distinct
# trace entries without committing to a specific field order.
restriction_digest <- function(r) {
  if (is.null(r)) return("none")
  paste(c(r$kind, r$var %||% "", r$level %||% ""), collapse = ":")
}

outcome_construction_digest <- function(o) {
  if (is.null(o)) return("none")
  paste(c(o$kind,
          paste(o$vars %||% character(), collapse = ","),
          o$covariate %||% "", o$baseline %||% "", o$factor %||% ""),
        collapse = ":")
}

# ---- model-form sampling + application -----------------------------
#
# Five sub-kinds. polynomial / spline are formula-level wraps applied
# inside materialise_spec(); the remaining three switch spec$family
# at fit time via apply_model_form(). Each candidate guards its own
# trigger so the search never enqueues a move that the data can't
# support (e.g. random_effect without a viable grouping factor).
#
#   polynomial(var, degree)   -> poly(var, degree) in RHS
#   spline(var, df)           -> splines::ns(var, df) in RHS
#   robust                    -> family$fitter = "rlm"
#   family_swap(family, link, outcome_coerce)
#                             -> family$fitter = "glm" with chosen family
#   random_effect(group)      -> family$fitter = "glmm" honoring group

sample_model_form <- function(df, outcome, spec) {
  candidates <- list()
  n <- nrow(df)
  y <- if (outcome %in% names(df)) df[[outcome]] else NULL

  nums_in_subset <- intersect(spec$subset, names(df))
  nums_in_subset <- nums_in_subset[vapply(nums_in_subset,
                                          function(p) is.numeric(df[[p]]) &&
                                            length(unique(df[[p]][is.finite(df[[p]])])) >= 4L,
                                          logical(1))]

  if (length(nums_in_subset)) {
    var <- if (length(nums_in_subset) == 1L) nums_in_subset
           else sample(nums_in_subset, 1L)
    degree <- sample(c(2L, 3L), 1L)
    candidates[[length(candidates) + 1L]] <- list(kind   = "polynomial",
                                                  var    = var,
                                                  degree = degree)
  }

  nums_for_spline <- nums_in_subset[vapply(nums_in_subset, function(p) {
    length(unique(df[[p]][is.finite(df[[p]])])) >= 10L
  }, logical(1))]
  if (length(nums_for_spline) && n >= 30L) {
    var <- if (length(nums_for_spline) == 1L) nums_for_spline
           else sample(nums_for_spline, 1L)
    candidates[[length(candidates) + 1L]] <- list(kind = "spline",
                                                  var  = var,
                                                  df   = 3L)
  }

  if (!is.null(y) && is.numeric(y) && n >= 20L) {
    candidates[[length(candidates) + 1L]] <- list(kind = "robust")
  }

  if (!is.null(y) && is.numeric(y) && n >= 30L) {
    swap_pool <- list(
      list(kind = "family_swap", family = "binomial", link = "logit",
           outcome_coerce = "median_dichotomize"),
      list(kind = "family_swap", family = "poisson",  link = "log",
           outcome_coerce = "round_clip")
    )
    finite_y <- y[is.finite(y)]
    if (length(finite_y) && all(finite_y > 0)) {
      swap_pool[[length(swap_pool) + 1L]] <- list(kind = "family_swap",
                                                  family = "Gamma",
                                                  link   = "log",
                                                  outcome_coerce = NULL)
    }
    candidates[[length(candidates) + 1L]] <- swap_pool[[
      sample.int(length(swap_pool), 1L)]]
  }

  factors <- categorical_columns_outside_subset(df, outcome, spec$subset)
  factors <- factors[vapply(factors, function(v) {
    x <- df[[v]]
    levs <- unique(x[!is.na(x)])
    length(levs) >= 3L && length(levs) <= max(3L, nrow(df) %/% 3L)
  }, logical(1))]
  if (length(factors) && !is.null(y) && is.numeric(y) && n >= 30L) {
    g <- if (length(factors) == 1L) factors else sample(factors, 1L)
    candidates[[length(candidates) + 1L]] <- list(kind  = "random_effect",
                                                  group = g)
  }

  if (!length(candidates)) return(NULL)
  candidates[[sample.int(length(candidates), 1L)]]
}

# Project a sampled model_form onto spec$family for fitter-switching
# sub-kinds (robust / family_swap / random_effect). polynomial and
# spline are RHS-only and leave spec$family alone -- the existing
# family selector still gets to pick lm vs gam vs whatever else.
apply_model_form <- function(spec, model_form = spec$model_form) {
  if (is.null(model_form)) return(spec)
  switch(model_form$kind,
    polynomial = spec,
    spline     = spec,
    robust     = {
      spec$family <- list(fitter = "rlm")
      spec
    },
    family_swap = {
      spec$family <- list(fitter = "glm",
                          family = model_form$family,
                          link   = model_form$link,
                          outcome_coerce = model_form$outcome_coerce)
      spec
    },
    random_effect = {
      spec$family <- list(fitter = "glmm",
                          family = "gaussian",
                          link   = "identity",
                          group  = model_form$group)
      spec
    },
    spec
  )
}

model_form_digest <- function(m) {
  if (is.null(m)) return("none")
  paste(c(m$kind,
          m$var %||% "",
          as.character(m$degree %||% m$df %||% ""),
          m$family %||% "", m$link %||% "",
          m$outcome_coerce %||% "",
          m$group %||% ""),
        collapse = ":")
}
