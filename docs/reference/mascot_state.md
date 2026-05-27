# Compute the mascot emotional state for a run snapshot.

Walks the progress ladder regardless of `best_p` so the player sees the
full emotional arc unfold during a run. The `resolved` face is reserved
for the final frame after a shippable run – it isn't returned mid-flight
from this function. `best_p` is kept on the signature for back-compat
callers but no longer short-circuits the ladder.

## Usage

``` r
mascot_state(progress, best_p = NA_real_, escalating = FALSE)
```

## Arguments

- progress:

  Numeric fraction of the run budget used (0 to 1).

- best_p:

  Numeric smallest p-value found so far in the run, or `NA` if none.
  Currently unused; retained for back-compat.

- escalating:

  Logical: is the derived-metric escalation phase currently active.

## Value

One of `"composed"`, `"uncertain"`, `"worried"`, `"anxious"`,
`"panicked"`, `"desperate"`.
