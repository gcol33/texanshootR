# Generate a graphical abstract PNG

A single-figure summary of the highlighted specification, composed as a
multi-panel layout with conceptual arrows. Renders via ggplot2 to PNG.

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

## Value

A length-one character vector giving the path to the written PNG file,
returned invisibly. Called for its side effect of generating the
graphical abstract in `output_dir` and advancing the publication chain.

## Details

Chain stage: **graphical_abstract** (length 5). See
[`progress()`](https://gillescolling.com/texanshootR/reference/progress.md).
