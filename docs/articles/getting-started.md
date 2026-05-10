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

## Unlocks

Only
[`shoot()`](https://gillescolling.com/texanshootR/reference/shoot.md)
and the inspection helpers are callable from session one. The six output
generators are gated by career tier and unlock as the career advances.
[`progress()`](https://gillescolling.com/texanshootR/reference/progress.md)
prints the live lock map:

``` r

progress()
```

The tier-to-function mapping:

| Tier | Unlocks |
|----|----|
| Junior Researcher | [`shoot()`](https://gillescolling.com/texanshootR/reference/shoot.md), [`career()`](https://gillescolling.com/texanshootR/reference/career.md), [`achievements()`](https://gillescolling.com/texanshootR/reference/achievements.md), [`wardrobe()`](https://gillescolling.com/texanshootR/reference/wardrobe.md), [`run_log()`](https://gillescolling.com/texanshootR/reference/run_log.md), [`progress()`](https://gillescolling.com/texanshootR/reference/progress.md), resets |
| Postdoc | [`manuscript()`](https://gillescolling.com/texanshootR/reference/manuscript.md), [`preprint()`](https://gillescolling.com/texanshootR/reference/preprint.md) |
| Senior Scientist | [`reviewer_response()`](https://gillescolling.com/texanshootR/reference/reviewer_response.md), [`graphical_abstract()`](https://gillescolling.com/texanshootR/reference/graphical_abstract.md), [`presentation()`](https://gillescolling.com/texanshootR/reference/presentation.md) |
| PI | [`funding()`](https://gillescolling.com/texanshootR/reference/funding.md) |

A locked call signals a `tx_locked` condition with a deadpan two-block
status message. From a fresh save:

    manuscript() requires:
    Postdoc

    Current career:
    Junior Researcher

Scripts can intercept the signal with
`tryCatch(manuscript(run), tx_locked = function(c) NULL)`. Inspect a
single function’s status with `progress("manuscript")`. Tier thresholds
are intentionally opaque; the path is more runs.

## Output generators

Each output function takes a `tx_run` and writes to
[`tempdir()`](https://rdrr.io/r/base/tempfile.html) by default. Override
with `output_dir` or `options(texanshootR.output_dir = ...)`. Once the
relevant tier is reached, the call works the same way for every
generator:

``` r

manuscript(run)
preprint(run)
reviewer_response(run)
graphical_abstract(run)
presentation(run)
funding(run)
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
