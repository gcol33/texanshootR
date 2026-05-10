# Generate a graphical abstract PNG

A single-figure summary of the highlighted specification, composed as a
deadpan multi-panel layout with conceptual arrows. Renders via ggplot2
to PNG.

## Usage

``` r
graphical_abstract(run, output_dir = NULL, file = NULL, force = FALSE)
```

## Arguments

- run:

  A `tx_run` object returned by
  [`shoot()`](https://gillescolling.com/texanshootR/reference/shoot.md).

- output_dir:

  Optional output directory.

- file:

  Optional filename stem (without extension).

- force:

  Overwrite an existing file.

## Details

Unlock requirement: **Senior Scientist**. See
[`progress()`](https://gillescolling.com/texanshootR/reference/progress.md)
for live state.
