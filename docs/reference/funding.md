# Generate a funding letter + budget DOCX

One-page Letter of Intent with a deadpan budget table.

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

## Details

Unlock requirement: **PI**. See
[`progress()`](https://gillescolling.com/texanshootR/reference/progress.md)
for live state.
