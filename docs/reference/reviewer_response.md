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
