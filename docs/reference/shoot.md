# Run an exploratory model search

Fits a battery of candidate specifications across predictor subsets,
transformations, interactions, outlier-removal seeds, and subgroup
seeds. Returns a `tx_run` object summarising the search and the
highlighted specification.

## Usage

``` r
shoot(
  df,
  formula = NULL,
  seed = NULL,
  escalate = TRUE,
  depth = c("default", "demo"),
  ...
)
```

## Arguments

- df:

  A data frame.

- formula:

  Optional formula. When `NULL`, the first numeric column is used as the
  outcome and all other columns as predictors.

- seed:

  Integer seed. When `NULL`, a random seed is generated and stored on
  the returned run.

- escalate:

  Logical. Allow the derived-metric escalation phase when the main
  search produces no spec with p \<= 0.05. Defaults to `TRUE`; the
  package leans into the parody.

- depth:

  `"default"` for the full search; `"demo"` for a single-fit smoke test
  used in CRAN-safe examples.

- ...:

  Reserved for future arguments.

## Value

A `tx_run` object (returned invisibly).
