# Generate a manuscript from a run

Renders an academic-style write-up of the highlighted specification. If
`quarto` and `tinytex` are available, produces a PDF + DOCX; if only
`rmarkdown` is available, produces a DOCX. The package never installs
LaTeX on its own.

## Usage

``` r
manuscript(run, output_dir = NULL, file = NULL, force = FALSE)
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

Character vector of file paths (invisible).
