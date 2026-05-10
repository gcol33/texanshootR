# texanshootR

<!-- badges: start -->
[![R-CMD-check](https://github.com/gcol33/texanshootR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/gcol33/texanshootR/actions/workflows/R-CMD-check.yaml)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

A roguelike-themed framework for exploratory linear-model search in R.

## Quick Start

```r
library(texanshootR)

run <- shoot(mtcars, budget = 30)
print(run)
summary(run)
```

A single call to `shoot()` fits a battery of candidate specifications
across predictor subsets, transformations, interactions, and
outlier-removal seeds, returning a `tx_run` with the highlighted
specification, the search summary, and any life events that fired.

## What it does

`shoot()` runs an exploratory search across a parameterised grid of
candidate specifications and surfaces the most defensible one. Around
that engine, the package layers a persistent career, an achievement
registry, ASCII cosmetics, and six output generators that turn a run
into something shippable: a manuscript skeleton, a preprint, a slide
deck, a reviewer-response letter, a graphical abstract, or a funding
letter.

The TUI is optional (`theatrical = FALSE`) and the search itself is
deterministic given a seed, with the seed, R version, package
version, and a hash of the search grid recorded on every run.

## Installation

```r
# Development version
# install.packages("pak")
pak::pak("gcol33/texanshootR")
```

## Features

### Search

```r
run <- shoot(mtcars, formula = mpg ~ ., budget = 30, seed = 42)
run_log()                          # full history of recorded runs
```

* `shoot()`: search across subsets, transforms, interactions, outlier-removal seeds, subgroup seeds.
* `depth = "demo"`: single-fit smoke test for CRAN-safe examples.
* `escalate = TRUE`: derived-metric escalation when no spec passes p &le; 0.05.

### Outputs

```r
manuscript(run)
preprint(run)
presentation(run)
graphical_abstract(run)
reviewer_response(run)
funding(run)
```

Each writes to `tempdir()` by default and returns the file path
invisibly. Override with `output_dir =` or
`options(texanshootR.output_dir = ...)`.

### Career, achievements, cosmetics

```r
career()        # level, runs, favorite method, opaque scores
achievements()  # registry of unlocked + still-hidden achievements
wardrobe()      # equipped cosmetic slots
```

State persists under `tools::R_user_dir("texanshootR", "data")`. The
first interactive save prompts before writing. Opt out with
`options(texanshootR.save_enabled = FALSE)`.

### Reset

```r
reset_career(force = TRUE)
reset_achievements(force = TRUE)
reset_wardrobe(force = TRUE)
reset_all(force = TRUE)
```

## Documentation

- [Getting Started](https://gillescolling.com/texanshootR/articles/getting-started.html)
- [Full Reference](https://gillescolling.com/texanshootR/reference/)
- [Message Pack Schema](MESSAGE_SCHEMA.md)
- [Contributing](CONTRIBUTING.md)

## Support

> "Where is the money, Lebowski?" &mdash; The Big Lebowski

I'm a PhD student who builds R packages in my free time because I
believe good tools should be free and open. I started these projects
for my own work and figured others might find them useful too.

If this package saved you some time, buying me a coffee is a nice way
to say thanks. It helps with my coffee addiction.

[![Buy Me A Coffee](https://img.shields.io/badge/-Buy%20me%20a%20coffee-FFDD00?logo=buymeacoffee&logoColor=black)](https://buymeacoffee.com/gcol33)

## License

MIT &copy; Gilles Colling. See [LICENSE](LICENSE).

## Citation

```bibtex
@software{texanshootR,
  author = {Colling, Gilles},
  title  = {texanshootR: Roguelike-Themed Framework for Exploratory Model Search},
  year   = {2026},
  url    = {https://github.com/gcol33/texanshootR}
}
```
