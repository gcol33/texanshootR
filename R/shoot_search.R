# Stateful, opportunistic spec generator.
#
# The shooter does not enumerate a clean cartesian product. He starts
# from a small seed pool (one spec per predictor subset, identity
# transforms, no perturbations) and then drifts toward whatever subset
# is producing the most encouraging p-values. Subsets that go cold
# get abandoned. Near-misses (best_p in (0.05, 0.20]) get re-perturbed
# along the other four dimensions. A wildcard is injected periodically
# to keep him moving when nothing is pulsing.
#
# This is desperation, not exploration. Each fork in the search is the
# shooter trying one more thing because the last thing was almost.
#
# Mechanic-to-fallacy contract (unchanged):
#   subsets       = data dredging / multiple comparisons
#   transforms    = researcher degrees of freedom
#   interactions  = p-hacking
#   outliers      = outlier exclusion
#   subgroups     = subgroup fishing

NEAR_MISS_RANGE       <- c(0.05, 0.20)
DEAD_AFTER_TRIES      <- 5L
DEAD_P_THRESHOLD      <- 0.5
WILDCARD_EVERY        <- 10L
PERTURBATIONS_PER_HIT <- 3L
PERTURB_KINDS         <- c("transform", "interaction", "outlier", "subgroup")

new_spec <- function(subset,
                     transforms    = NULL,
                     interactions  = character(),
                     outlier_seed  = "none",
                     subgroup_seed = "none",
                     source        = "seed",
                     family        = list(fitter = "lm")) {
  if (is.null(transforms)) {
    transforms <- stats::setNames(rep("identity", length(subset)), subset)
  }
  list(subset        = subset,
       transforms    = transforms,
       interactions  = interactions,
       outlier_seed  = outlier_seed,
       subgroup_seed = subgroup_seed,
       source        = source,
       family        = family)
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
  } else if (kind == "outlier") {
    s$outlier_seed <- sample(c("leverage_top_5pct", "random_5pct"), 1L)
  } else if (kind == "subgroup") {
    s$subgroup_seed <- sample(c("halves_a", "halves_b"), 1L)
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

# Apply transformations + outlier/subgroup seeds to a data frame and
# return the modified copy plus the formula string for the spec.
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

  d <- apply_outlier_seed(d, outcome, spec$outlier_seed)
  d <- apply_subgroup_seed(d, spec$subgroup_seed)

  rhs <- if (length(spec$subset) == 0L) "1"
         else paste(spec$subset, collapse = " + ")
  if (any(nzchar(spec$interactions))) {
    its <- spec$interactions[nzchar(spec$interactions)]
    rhs <- paste(rhs, paste(its, collapse = " + "), sep = " + ")
  }
  formula_str <- paste(outcome, "~", rhs)

  list(df = d, formula = formula_str)
}

apply_outlier_seed <- function(d, outcome, seed) {
  if (seed == "none") return(d)
  if (!(outcome %in% names(d))) return(d)
  if (!is.numeric(d[[outcome]])) return(d)
  n <- nrow(d)
  if (n < 20L) return(d)
  drop_n <- max(1L, as.integer(0.05 * n))
  if (seed == "leverage_top_5pct") {
    y <- d[[outcome]]
    z <- abs((y - mean(y, na.rm = TRUE)) / stats::sd(y, na.rm = TRUE))
    drop_idx <- order(z, decreasing = TRUE)[seq_len(drop_n)]
  } else {  # random_5pct
    drop_idx <- sample.int(n, drop_n)
  }
  d[-drop_idx, , drop = FALSE]
}

apply_subgroup_seed <- function(d, seed) {
  if (seed == "none") return(d)
  n <- nrow(d)
  if (n < 30L) return(d)
  half <- floor(n / 2L)
  if (seed == "halves_a") d[seq_len(half), , drop = FALSE]
  else if (seed == "halves_b") d[(half + 1L):n, , drop = FALSE]
  else d
}
