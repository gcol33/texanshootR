# Browse persisted run history

Returns a data.frame of recent runs with one row per run. The full run
record can be loaded via `attr(run_log(), "records")[[i]]`.

## Usage

``` r
run_log(n = 25L)
```

## Arguments

- n:

  Maximum number of runs to list.

## Value

A data.frame with one row per run.
