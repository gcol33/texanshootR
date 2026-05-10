# Getting started with texanshootR

`texanshootR` provides a structured, terminal-first interface for
exploratory linear-model search. The main entry point is
[`shoot()`](https://gillescolling.com/texanshootR/reference/shoot.md).

## A first run

``` r

library(texanshootR)

run <- shoot(mtcars)
print(run)
summary(run)
```

The returned `tx_run` object stores the highlighted specification, the
search summary, and any life events that fired during the run.

## The career

The package keeps a persistent career across runs in
`tools::R_user_dir("texanshootR", "data")`. Use
[`career()`](https://gillescolling.com/texanshootR/reference/career.md)
to see your current state:

``` r

career()
```

The first time you save, you’ll be prompted to confirm the path. Set
`options(texanshootR.save_enabled = FALSE)` to opt out entirely.

## Output generators

Each output function takes a `tx_run` and writes to
[`tempdir()`](https://rdrr.io/r/base/tempfile.html) by default. Override
with `output_dir` or `options(texanshootR.output_dir = ...)`:

``` r

graphical_abstract(run)
manuscript(run)
preprint(run)
reviewer_response(run)
funding(run)
presentation(run)
```

Output functions return the file path invisibly and print a short status
line.

## Achievements and cosmetics

``` r

achievements()
wardrobe()
```

Hidden achievements appear as `???` until unlocked. New cosmetics
auto-equip when their unlocking achievement fires; override the
selection with `wardrobe(slot, id)`.

## Resetting

If you want to start over:

``` r

reset_career(force = TRUE)
reset_all(force = TRUE)
```
