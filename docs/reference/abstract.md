# Generate a one-paragraph deadpan abstract

The entry-tier output. Always the first stage of the publication chain
(see
[`progress()`](https://gillescolling.com/texanshootR/reference/progress.md)).
Writes a plain-text `.txt` file describing the highlighted specification
in the unbothered register typical of an applied-stats abstract.

## Usage

``` r
abstract(run, output_dir = NULL, file = NULL, force = FALSE)
```

## Arguments

- run:

  A `tx_run` object returned by
  [`shoot()`](https://gillescolling.com/texanshootR/reference/shoot.md).
  Must be the currently active publication chain — i.e., the run that
  opened the chain when it cleared `p <= 0.05`.

- output_dir:

  Optional output directory.

- file:

  Optional filename stem (without extension).

- force:

  Overwrite an existing file.

## Value

Character path to the written file (invisible).

## Details

Chain stage: **abstract** (length 1, always unlocked).
