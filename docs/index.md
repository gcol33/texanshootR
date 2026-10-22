# texanshootR

*running tests until one comes back significant*

![texanshootR mascot: a sad cowboy in a terminal cycling through
emotional states as the p-value drops](reference/figures/mascot.svg)

texanshootR runs a budgeted, seeded specification search over a data
frame until some model reaches `p <= 0.05`, then writes the paper around
it.

The name comes from the [Texas
sharpshooter](https://en.wikipedia.org/wiki/Texas_sharpshooter_fallacy),
who fires at the side of a barn all night and in the morning paints a
target around the densest cluster of holes. Each
[`shoot()`](https://gillescolling.com/texanshootR/reference/shoot.md)
call opens a wall-clock budget and searches predictor subsets,
transformations, interactions, outlier exclusions and subgroup splits.
As the budget runs low it moves on to other model families and derived
metrics, favouring whatever is almost significant and abandoning
specifications that go cold. The seed, R version, package version and a
hash of the full search trace are stored on the returned run, so you can
replay any saved run exactly and decide which finding to write up.

texanshootR supports:

- **Searching the model space.** Eight model families have native
  fitters, with the heavier ones written in C++ through
  [Rcpp](https://www.rcpp.org/), and a selector escalates from `lm` to
  the others as the run comes under pressure ([The search
  engine](#the-search-engine)).
- **Writing it up.** A run that clears `p <= 0.05` opens a six-stage
  publication chain: abstract, manuscript, presentation, reviewer
  response, graphical abstract and funding letter.
- **A research career.** Redeemed stages award XP, and the career tier
  that follows from it decides which model families enter the search.
  Achievements and a wardrobe of cosmetics come with it.
- **Commentary in the terminal.** A YAML message registry, each message
  tagged with the fallacy it commits, narrates the search.

## Installation

The release on CRAN installs as a binary on Windows and macOS, so no
build tools are needed:

``` r

install.packages("texanshootR")
```

The development version compiles C++ code, so install the build tools
for your operating system first:

| Operating system | Build tools |
|----|----|
| Windows | [Rtools](https://cran.r-project.org/bin/windows/Rtools/), the version that matches your R version |
| macOS | Run `xcode-select --install` in Terminal |
| Debian / Ubuntu | Install `r-base-dev` |

Check that R can find the compiler:

``` r

pkgbuild::has_build_tools(debug = TRUE)
```

Then install texanshootR:

``` r

# install.packages("pak")
pak::pak("gcol33/texanshootR")
```

The C++ backend (penalised least squares for `gam`, a profile-likelihood
mixed model for `glmm`) needs no system libraries; its only compiled
dependency is Rcpp, which installs from CRAN with the package.

## Usage

1.  Run the search on a data frame with
    [`shoot()`](https://gillescolling.com/texanshootR/reference/shoot.md).
2.  Print the run to see the finding, the search behind it and the
    outputs it unlocked.
3.  Redeem the unlocked stages of the publication chain, starting with
    [`abstract()`](https://gillescolling.com/texanshootR/reference/abstract.md).

``` r

library(texanshootR)

run <- shoot(mtcars)
#> A finding has cleared threshold. Pipeline armed.
#> Publication chain opened. Next: abstract().
run
#> ================================================================
#> ( -_-) ︻デ═一  ·
#> texanshootR :: run 0001
#> ================================================================
#> 
#> RUN
#> status: publishable result identified
#> career impact: advanced
#> mascot state: composed → resolved
#> 
#> FINDING
#> mpg ~ gear + drat
#> methods: partialling out variation due to am
#> p: 0.045
#> 
#> SEARCH
#> candidate specifications: 3,000
#> model families explored: lm glm rlm
#> modifiers used: +compilation +complexity_shield
#> 
#> OUTPUTS
#> available now: abstract(run)
#> 
#> locked:
#> manuscript()         requires Postdoc
#> presentation()       requires Postdoc
#> reviewer_response()  requires Senior Scientist
#> graphical_abstract() requires Senior Scientist
#> funding()            requires PI
#> 
#> DATA USED
#> exclusion rule: none
#> rows excluded: 0
#> outcome construction: partialling out variation due to am
#> derived metrics: disabled
#> search seed: 1140350788
#> run stored: no
```

Without a formula,
[`shoot()`](https://gillescolling.com/texanshootR/reference/shoot.md)
takes the first numeric column as the outcome and every other column as
a predictor. The default budget is 30 seconds of wall-clock time, so a
run on another machine may land somewhere else; the stored seed replays
it.

``` r

abstract(run)
```

Each generator writes its file to
[`tempdir()`](https://rdrr.io/r/base/tempfile.html) and returns the path
invisibly. `output_dir =` or `options(texanshootR.output_dir = ...)`
sends it elsewhere.

## The search engine

The seven families of the career ladder each have their own fitter, with
no soft dependency on mgcv, lme4 or lavaan:

- `lm`: ordinary least squares via
  [`.lm.fit()`](https://rdrr.io/r/stats/lmfit.html)
- `cor`: bivariate Pearson, Spearman or Kendall correlation
- `glm`: Gaussian, binomial, Poisson or Gamma with a choice of links
- `wls`: two-stage feasible weighted least squares
- `gam`: penalised B-spline regression with smoothing chosen by GCV, in
  C++
- `glmm`: Gaussian random-intercept mixed model by profile likelihood,
  in C++
- `sem`: single-mediator path model with a Sobel test for the indirect
  effect

Model-form escalation can also refit a specification with `rlm`, robust
M-estimation with Huber weights. A family selector attaches a fitter to
each specification from the run’s escalation state and the shape of the
outcome: `lm` early, `glm` under pressure, and an occasional forced
coercion (binomial on a continuous outcome, Poisson on a rounded one)
when the shooter is chasing the next decimal.

## The publication chain

A run that clears `p <= 0.05` is shippable, and the first shippable run
opens the chain. The stages are redeemed in order:

``` r

abstract(run)            # one-paragraph deadpan summary
manuscript(run)          # IMRaD draft; Methods match the winning spec
presentation(run)        # 8-slide deck; residual plot on slide 7
reviewer_response(run)   # opens "we thank the reviewer for their thoughtful comments"
graphical_abstract(run)  # the figure your PI will retweet
funding(run)             # the next grant, citing the just-shipped finding
```

Finishing the chain through your unlocked stages adds a length bonus to
the XP of each stage. A fresh
[`shoot()`](https://gillescolling.com/texanshootR/reference/shoot.md)
mid-chain forfeits the bonus and keeps the partial XP. Locked stages,
expired windows, the wrong run and out-of-order calls signal a
`tx_chain_error` whose `reason` field your tests can branch on. [Getting
started with
texanshootR](https://gillescolling.com/texanshootR/articles/getting-started.html#the-publication-chain)
walks through a chain.

## Career and unlocks

Cumulative XP sets how many stages of the chain you can redeem, and your
career tier is a label derived from that length. The tier gates which
model families enter the search pool:

| Chain length | New stage | XP needed | Career tier | Families added |
|----|----|----|----|----|
| 1 | [`abstract()`](https://gillescolling.com/texanshootR/reference/abstract.md) | 0 | Junior Researcher | `lm` |
| 2 | [`manuscript()`](https://gillescolling.com/texanshootR/reference/manuscript.md) | 5 | Postdoc | `cor`, `glm` |
| 3 | [`presentation()`](https://gillescolling.com/texanshootR/reference/presentation.md) | 15 | Postdoc | none |
| 4 | [`reviewer_response()`](https://gillescolling.com/texanshootR/reference/reviewer_response.md) | 30 | Senior Scientist | `wls`, `gam` |
| 5 | [`graphical_abstract()`](https://gillescolling.com/texanshootR/reference/graphical_abstract.md) | 55 | Senior Scientist | none |
| 6 | [`funding()`](https://gillescolling.com/texanshootR/reference/funding.md) | 90 | PI | `glmm`, `sem` |

``` r

career()        # tier, runs, favourite method, opaque scores
achievements()  # 20 unlockable badges; hidden ones show as ???
wardrobe()      # cosmetic slots (hat, badge, cloak, poncho, lanyard)
progress()      # HUD: chain length, XP, next unlock, live chain window
run_log()       # every run on this profile
```

## The message pack

The terminal commentary comes from a YAML message registry under
`inst/messages/`. Each message carries a trigger phase, a fallacy tag
from `vocab_tags` (such as `p_hacking`, `harking`, `subgroup_fishing`
and `causal_overreach`), a rarity weight, and optional affinities to a
mascot state and a model family, so a line about GAMs only fires when
the run picked a GAM.
[`validate_messages()`](https://gillescolling.com/texanshootR/reference/validate_messages.md)
checks the registry against the schema in
[MESSAGE_SCHEMA.md](https://github.com/gcol33/texanshootR/blob/main/MESSAGE_SCHEMA.md),
so adding a message is a YAML edit and a call to it.

## Persistent state

Your researcher profile is stored as flat YAML under
`tools::R_user_dir("texanshootR", "data")`, readable and
version-controllable. The first interactive save asks before writing
anything to disk. With `options(texanshootR.save_enabled = FALSE)` the
package runs without state: every call is independent, and progression
stays at Junior Researcher.
[`reset_career()`](https://gillescolling.com/texanshootR/reference/reset.md),
[`reset_achievements()`](https://gillescolling.com/texanshootR/reference/reset.md),
[`reset_wardrobe()`](https://gillescolling.com/texanshootR/reference/reset.md)
and
[`reset_all()`](https://gillescolling.com/texanshootR/reference/reset.md)
start over
([Resetting](https://gillescolling.com/texanshootR/articles/getting-started.html#resetting)).

## Further reading

Brodeur, A., Cook, N. and Heyes, A. (2020). *Methods Matter: P-Hacking
and Publication Bias in Causal Analysis in Economics.* American Economic
Review 110(11): 3634-60. <https://doi.org/10.1257/aer.20190687>

## Documentation

Start with [Getting started with
texanshootR](https://gillescolling.com/texanshootR/articles/getting-started.html):
a first run, the publication chain, the career and how to reset it.

- [Message pack
  schema](https://github.com/gcol33/texanshootR/blob/main/MESSAGE_SCHEMA.md):
  the fields of a message and the tag vocabulary.
- [Full reference](https://gillescolling.com/texanshootR/reference/):
  every function.

## Getting help

Questions and bug reports go to the [GitHub
issues](https://github.com/gcol33/texanshootR/issues). A bug report is
easiest to act on with a small reproducible example, for instance made
with [reprex](https://reprex.tidyverse.org/).

## Contributing

Pull requests are welcome; see the [contributing
guide](https://gillescolling.com/texanshootR/CONTRIBUTING.html).

## Support

> “Software is like sex: it’s better when it’s free.” — Linus Torvalds

I’m a PhD student who builds R packages in my free time because I
believe good tools should be free and open. I started these projects for
my own work and figured others might find them useful too.

If this package saved you some time, buying me a coffee is a nice way to
say thanks. It helps with my coffee addiction.

[![Buy Me A
Coffee](https://img.shields.io/badge/-Buy%20me%20a%20coffee-FFDD00?logo=buymeacoffee&logoColor=black)](https://buymeacoffee.com/gcol33)

## License

texanshootR is licensed under MIT.

## Citation

``` text
Colling G (2026). _texanshootR: Reproducible Audit Trails for
Indefensible Research_. doi:10.32614/CRAN.package.texanshootR
<https://doi.org/10.32614/CRAN.package.texanshootR>. R package version
0.1.1, <https://gillescolling.com/texanshootR/>.
```

BibTeX:

``` bibtex
@Manual{colling2026texanshootr,
  title = {texanshootR: Reproducible Audit Trails for Indefensible Research},
  author = {Gilles Colling},
  year = {2026},
  note = {R package version 0.1.1},
  url = {https://gillescolling.com/texanshootR/},
  doi = {10.32614/CRAN.package.texanshootR},
}
```

Download the citation as
[BibTeX](https://gillescolling.com/texanshootR/texanshootR.bib) or
[RIS](https://gillescolling.com/texanshootR/texanshootR.ris).
