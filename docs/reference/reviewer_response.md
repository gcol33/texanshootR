# Generate a response-to-reviewers DOCX

Polite, point-by-point response to imagined reviewer comments.

## Usage

``` r
reviewer_response(run, output_dir = NULL, file = NULL, force = FALSE)
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
returned invisibly. Called for its side effect of generating the
response-to-reviewers document in `output_dir` and advancing the
publication chain.

## Details

Chain stage: **reviewer_response** (length 4). See
[`progress()`](https://gillescolling.com/texanshootR/reference/progress.md).
