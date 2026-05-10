# Build the forking-paths search grid.
#
# Every dimension maps to a row in the fallacy/mechanic contract:
#   subsets       = data dredging / multiple comparisons
#   transforms    = researcher degrees of freedom
#   interactions  = p-hacking
#   outliers      = outlier exclusion
#   subgroups     = subgroup fishing
#
# A "spec" is a (predictor_subset, transform_map, interaction_set,
# outlier_seed, subgroup_seed) tuple. The grid is generated lazily so
# we can cap by `budget * per_spec_seconds` without enumerating
# everything up front.

build_search_grid <- function(df, outcome, predictors,
                              max_subset_size = 4L,
                              transform_set = c("identity", "log", "sqrt", "scale"),
                              interaction_top_k = 3L,
                              outlier_seeds = c("none", "leverage_top_5pct", "random_5pct"),
                              subgroup_seeds = c("none", "halves_a", "halves_b"),
                              cap = 5000L) {

  predictors <- intersect(predictors, names(df))
  predictors <- setdiff(predictors, outcome)

  is_num <- vapply(predictors, function(p) is.numeric(df[[p]]), logical(1))
  numeric_preds <- predictors[is_num]

  # Predictor subsets: 1..k combinations, capped.
  ks <- seq_len(min(max_subset_size, length(predictors)))
  subset_list <- list()
  for (k in ks) {
    if (length(predictors) >= k) {
      combs <- utils::combn(predictors, k, simplify = FALSE)
      subset_list <- c(subset_list, combs)
    }
  }
  subset_list <- utils::head(subset_list, max(1L, cap %/% 4L))

  # Transformation maps: per-numeric-predictor choice from transform_set.
  # To keep the grid bounded we sample, not enumerate, the cartesian
  # product. One identity map plus a handful of perturbations per subset.
  transform_maps <- function(numeric_in_subset) {
    if (length(numeric_in_subset) == 0L) return(list(stats::setNames(character(), character())))
    out <- list(stats::setNames(rep("identity", length(numeric_in_subset)), numeric_in_subset))
    for (t in transform_set[-1]) {
      m <- stats::setNames(rep("identity", length(numeric_in_subset)), numeric_in_subset)
      m[1] <- t
      out[[length(out) + 1L]] <- m
    }
    out
  }

  # Top-k pairwise interactions, by univariate effect strength.
  interaction_candidates <- top_interactions(df, outcome, numeric_preds,
                                              top_k = interaction_top_k)

  grid <- list()
  for (subset in subset_list) {
    in_subset_num <- intersect(subset, numeric_preds)
    tmaps <- transform_maps(in_subset_num)
    ints <- list(character())
    for (cand in interaction_candidates) {
      if (all(cand %in% subset)) {
        ints[[length(ints) + 1L]] <- paste(cand, collapse = ":")
      }
    }
    for (tm in tmaps) {
      for (it in ints) {
        for (oseed in outlier_seeds) {
          for (gseed in subgroup_seeds) {
            grid[[length(grid) + 1L]] <- list(
              subset = subset,
              transforms = tm,
              interactions = it,
              outlier_seed = oseed,
              subgroup_seed = gseed
            )
            if (length(grid) >= cap) break
          }
          if (length(grid) >= cap) break
        }
        if (length(grid) >= cap) break
      }
      if (length(grid) >= cap) break
    }
    if (length(grid) >= cap) break
  }

  grid
}

# Identify the top-k pairwise interactions ranked by the absolute
# bivariate correlation of the product term with the outcome. Cheap
# proxy for which interactions are worth including in the search.
top_interactions <- function(df, outcome, numeric_preds, top_k = 3L) {
  if (length(numeric_preds) < 2L) return(list())
  y <- df[[outcome]]
  if (!is.numeric(y)) return(list())
  pairs <- utils::combn(numeric_preds, 2L, simplify = FALSE)
  scores <- vapply(pairs, function(p) {
    prod <- df[[p[1]]] * df[[p[2]]]
    if (sd(prod, na.rm = TRUE) == 0) return(0)
    abs(stats::cor(prod, y, use = "complete.obs"))
  }, numeric(1))
  ord <- order(scores, decreasing = TRUE)
  ord <- ord[seq_len(min(top_k, length(ord)))]
  pairs[ord]
}

# Apply transformations + outlier/subgroup seeds to a data frame and
# return the modified copy plus the formula string for the spec.
materialise_spec <- function(df, outcome, spec) {
  d <- df

  # Transformations.
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

  # Outlier seeds.
  d <- apply_outlier_seed(d, outcome, spec$outlier_seed)

  # Subgroup seeds.
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
