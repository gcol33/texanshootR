# Run an exploratory model search

Fits a battery of candidate specifications across predictor subsets,
transformations, interactions, principled-sounding sample restrictions
(complete cases, IQR fences, Cook's D, factor-level restrictions), and
outcome-engineering moves (composite indices, residualisation, ratios,
within-group z-scoring). Returns a `tx_run` object summarising the
search and the highlighted specification.

## Usage

``` r
shoot(
  df,
  formula = NULL,
  seed = NULL,
  depth = c("default", "demo"),
  terminal = FALSE,
  interactive_modifiers = FALSE,
  abstract = FALSE,
  manuscript = FALSE,
  presentation = FALSE,
  reviewer_response = FALSE,
  graphical_abstract = FALSE,
  funding = FALSE,
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

- depth:

  `"default"` for the full search; `"demo"` for a single-fit smoke test
  used in CRAN-safe examples.

- terminal:

  When `TRUE`, spawn an external Windows Terminal (or fallback console
  window) and run `shoot()` inside it so the full ANSI multi-zone TUI
  renders. Useful when calling from RStudio Console, which only supports
  `\r`-overwrite and so falls back to the compressed single-line status.
  The result is returned to the calling session as usual. Windows-only.

- interactive_modifiers:

  When `TRUE`, open a
  [`readline()`](https://rdrr.io/r/base/readline.html) tactical-prompt
  window at each mascot transition so the player can type `+command`
  modifiers live. Default `FALSE`: modifiers are pre-rolled at run start
  and applied autonomously, which keeps the in-place animation intact in
  RStudio's console.

- abstract, manuscript, presentation, reviewer_response,
  graphical_abstract, funding:

  Logical flags. When `TRUE` and the run is shippable, auto-generate the
  matching output and every chain stage required to reach it. Default
  `FALSE`.

- ...:

  Reserved for future arguments and aliases. Recognised aliases:
  `powerpoint` (= `presentation`), `reviewer` (= `reviewer_response`),
  `graphical` (= `graphical_abstract`). Internal: `prompt_fn` can be
  supplied to drive tactical-pause input non-interactively (tests).

## Value

A `tx_run` object. Returned visibly so the
[`print()`](https://rdrr.io/r/base/print.html) banner fires at the
prompt; the demo path (`depth = "demo"`) returns invisibly because there
is no banner worth showing for a one-fit smoke test.

## Details

Every shoot has a wall-clock budget (default 30s). Modifiers are
pre-rolled at run start by default: shoot() picks a random per-
transition loadout (`+glmm`, `+derived_metrics`, ...) and applies each
one when the matching mascot state transition fires, extending the
deadline by the modifier's per-token bonus (2-5s) and redirecting the
search priority. Pass `interactive_modifiers = TRUE` to opt into the
readline tactical-prompt window at each transition instead. Without
`+derived_metrics`, runs that fail to clear `p <= 0.05` simply lose —
the highlighted spec stays above threshold and the output gauntlet does
not open.

Output flags (`abstract`, `manuscript`, `presentation` / `powerpoint`,
`reviewer_response` / `reviewer`, `graphical_abstract` / `graphical`,
`funding`) auto-generate the matching file when the run is shippable.
The chain prefix needed to reach the highest enabled flag is generated
in order: e.g. `presentation = TRUE` produces abstract, manuscript, and
presentation.
