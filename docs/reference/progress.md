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
  achievement / wardrobe counts, and in-flight progress for achievements
  with public thresholds.

- `progress("manuscript")` – per-function card for a gated function.

- `progress("ach_multiple_comparisons")` – per-achievement card.

Pairs with the static unlock requirement printed in each gated
function's help page (e.g.
[`?presentation`](https://gillescolling.com/texanshootR/reference/presentation.md)).
The help page documents the requirement; `progress()` reports the live
state. Career-tier distance stays opaque by design – only qualitative
tier names are surfaced, never the underlying score.
