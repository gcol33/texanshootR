# Generate a preprint HTML

bioRxiv-style HTML rendering of a run's highlighted specification.
Includes a fake DOI and a deadpan "Comments (0)" footer.

## Usage

``` r
preprint(run, output_dir = NULL, file = NULL, force = FALSE)
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
