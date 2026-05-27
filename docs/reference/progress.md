# Inspect API unlock progress

Prints a HUD-style summary of unlock state for gated functions and
achievements. Reads live save state.

## Usage

``` r
progress(what = NULL)
```

## Arguments

- what:

  Optional. A gated-function name (e.g. `"manuscript"`) or an
  achievement id (e.g. `"ach_multiple_comparisons"`). When NULL, prints
  the overview.

## Value

A `tx_progress` object (invisible).

## Details

Three call modes:

- `progress()` – overview: career tier, gated-function lock map,
  achievement / wardrobe counts, and in-flight progress.

- `progress("manuscript")` – per-function card.

- `progress("ach_multiple_comparisons")` – per-achievement card.
