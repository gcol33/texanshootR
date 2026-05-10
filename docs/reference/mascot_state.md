# Compute the mascot emotional state for a run snapshot.

Compute the mascot emotional state for a run snapshot.

## Usage

``` r
mascot_state(progress, best_p = NA_real_, escalating = FALSE)
```

## Arguments

- progress:

  Numeric fraction of the run budget used (0 to 1).

- best_p:

  Numeric smallest p-value found so far in the run, or `NA` if none.

- escalating:

  Logical: is the derived-metric escalation phase currently active.

## Value

One of `"composed"`, `"uncertain"`, `"anxious"`, `"desperate"`,
`"resolved"`.
