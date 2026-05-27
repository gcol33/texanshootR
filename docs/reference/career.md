# Inspect career state

Returns a `tx_career` object describing the persistent career, or a
fresh default if no save exists. The object's `print` method shows a
compact dashboard card; `summary` adds aggregate counts. Career tier is
derived from the publication-chain length you've unlocked (see
[`progress()`](https://gillescolling.com/texanshootR/reference/progress.md));
XP comes from completed chain stages.

## Usage

``` r
career()
```

## Value

A `tx_career` object.
