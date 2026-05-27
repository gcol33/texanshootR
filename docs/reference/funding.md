# Generate a funding letter + budget DOCX

One-page Letter of Intent with a costed budget table.

## Usage

``` r
funding(run, output_dir = NULL, file = NULL, force = FALSE)
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

A length-one character vector giving the path to the written DOCX file,
returned invisibly. Called for its side effect of generating the funding
letter in `output_dir` and advancing the publication chain.

## Details

Chain stage: **funding** (length 6). See
[`progress()`](https://gillescolling.com/texanshootR/reference/progress.md).
