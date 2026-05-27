# Generate a chaotic conference-style PPTX

Builds a multi-slide PowerPoint via `officer`. v1 produces the
structural chaos (rotated text, gradient fills, irrelevant clipart,
shape collisions); slide-transition and per-element animation XML
injection lands in a follow-up.

## Usage

``` r
presentation(run, output_dir = NULL, file = NULL, force = FALSE)
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

A length-one character vector giving the path to the written PPTX file,
returned invisibly. Called for its side effect of generating the slide
deck in `output_dir` and advancing the publication chain.

## Details

Chain stage: **presentation** (length 3). See
[`progress()`](https://gillescolling.com/texanshootR/reference/progress.md).
