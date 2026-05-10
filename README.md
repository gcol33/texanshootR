# texanshootR

<!-- badges: start -->
[![R-CMD-check](https://github.com/gcol33/texanshootR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/gcol33/texanshootR/actions/workflows/R-CMD-check.yaml)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

<p align="center">
  <img src="man/figures/mascot.svg" alt="texanshootR mascot — a sad cowboy in a terminal cycling through emotional states as the p-value drops" width="480">
</p>

> **Mission.** To contribute to dubious research and questionable p-values.
>
> In this day and age where *publish or perish* reigns king, a lone shooter
> helps you out in your predicament.

A roguelike-themed framework for exploratory linear-model search in R.

## Quick start

```r
library(texanshootR)

run <- shoot(mtcars)
print(run)
summary(run)
```

The Texas sharpshooter fires at the side of a barn, then walks over and paints
the target around the densest cluster of bullet holes. `shoot()` automates the
firing — across predictor subsets, transformations, interactions, outlier
exclusions, and subgroup splits — and surfaces the most defensible result. If
nothing clears `p ≤ 0.05`, the run escalates to derived metrics, which is what
you would have done anyway. There is now a banner about it.

The TUI is optional (`theatrical = FALSE`). Every run is deterministic given a
seed, and the seed, R version, package version, and a hash of the search grid
are recorded on the returned object — so the *audit trail* is honest even when
the *intent* is not.

## Outputs

A finished run is a `tx_run`. Six generators turn it into something shippable:

```r
manuscript(run)         # IMRaD draft, Methods that match the *winning* spec
preprint(run)           # arXiv-flavoured, with the limitations section pre-hedged
presentation(run)       # 8-slide deck (slide 7 contains the only honest figure)
graphical_abstract(run) # the figure your PI will retweet
reviewer_response(run)  # opens with "we thank the reviewer for their thoughtful comments"
funding(run)            # the next grant, citing the just-shipped finding
```

Each writes to `tempdir()` by default and returns the file path invisibly.
Override with `output_dir =` or `options(texanshootR.output_dir = ...)`.

## Career, achievements, cosmetics

Every run feeds a persistent profile, because the joke does not work without
progression.

```r
career()        # level, runs, favourite method, opaque scores
achievements()  # 20 unlockable badges; hidden ones show as ???
wardrobe()      # equipped cosmetic slots (hat, badge, cloak, poncho, lanyard)
```

State persists under `tools::R_user_dir("texanshootR", "data")`. The first
interactive save prompts before writing anything to disk. Opt out entirely
with `options(texanshootR.save_enabled = FALSE)`.

## Reset

For when the simulation ends and you would like a new one:

```r
reset_career(force = TRUE)
reset_achievements(force = TRUE)
reset_wardrobe(force = TRUE)
reset_all(force = TRUE)
```

## Installation

```r
# install.packages("pak")
pak::pak("gcol33/texanshootR")
```

## Documentation

* [Getting Started](https://gillescolling.com/texanshootR/articles/getting-started.html)
* [Full Reference](https://gillescolling.com/texanshootR/reference/)
* [Message Pack Schema](MESSAGE_SCHEMA.md)
* [Contributing](CONTRIBUTING.md)

## Further reading

This package is a parody. The phenomenon it parodies is not.

Brodeur, A., Cook, N. and Heyes, A. (2020).
*Methods Matter: P-Hacking and Publication Bias in Causal Analysis in
Economics.* **American Economic Review** 110(11): 3634–60.
<https://doi.org/10.1257/aer.20190687>

If `shoot()` feels uncomfortably close to a real workflow, that paper is a
better starting point than the achievement registry.

## Support

> "Where is the money, Lebowski?"
> &mdash; The Big Lebowski

I'm a PhD student who builds R packages in my free time because I believe good
tools should be free and open. I started these projects for my own work and
figured others might find them useful too.

If this package saved you some time — or pre-empted a fit of overfitting —
buying me a coffee is a nice way to say thanks.

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
