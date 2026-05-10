# texanshootR

<!-- badges: start -->
[![R-CMD-check](https://github.com/gcol33/texanshootR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/gcol33/texanshootR/actions/workflows/R-CMD-check.yaml)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

<p align="center">
  <img src="man/figures/mascot.gif" alt="texanshootR mascot — a cowboy in a terminal cycling through composure, dread, and derived metrics as the p-value drops" width="480">
</p>

> **Mission.** A reproducible audit trail for research that does not, in
> retrospect, deserve one.
>
> *Publish or perish* reigns king. A lone shooter is here to help. He has
> shot many barns. He will shoot yours.

Every `shoot()` is one barn, one shooter, one budget. He starts composed —
theory-led predictors, no transformations, a single honest model. By the
fiftieth attempt he is uncertain, dropping a covariate, logging the response.
By two hundred he is worried, excluding outliers and discovering subgroups.
With the budget almost out he is desperate, escalating to a derived metric
while the word *exploratory* works forty hours of overtime. Either
`p ≤ 0.05` lands somewhere on the wall or it does not. There is a banner
about it either way.

## Quick start

```r
library(texanshootR)

run <- shoot(mtcars)
print(run)
summary(run)
```

`shoot()` flails — opportunistically, not systematically — across predictor
subsets, transformations, interactions, outlier exclusions, subgroup splits,
and (at higher career tiers) model families, biasing toward whatever is
*almost* significant and abandoning specs that go cold. The print method
leads with the shooter's face and the arc he followed
(`composed -> resolved`, `panicked -> resolved (last-minute)`,
`escalated to derived metrics`, etc.) before the formula and the p-value.

Every run is deterministic given a seed, and the seed, R version, package
version, and a hash of the search trace are recorded on the returned object —
so the *audit trail* is honest even when the *intent* is not.

## What's in the box

`texanshootR` is a terminal-first model-search engine wrapped in a
career-progression game. The two halves are fully wired together.

### The search engine

* **Seven model families**, each with a native fitter (no soft dependencies on
  `mgcv`, `lme4`, or `lavaan` — the heavy ones are implemented in C++ via
  `Rcpp`):
  * `lm` — ordinary least squares via `.lm.fit()`
  * `cor` — bivariate Pearson / Spearman / Kendall
  * `glm` — Gaussian / binomial / Poisson / Gamma with link choices
  * `wls` — two-stage feasible weighted least squares
  * `gam` — penalised B-spline regression with GCV smoothing-parameter selection
  * `glmm` — Gaussian random-intercept LMM with profile-likelihood estimation
  * `sem` — single-mediator path model with Sobel test for the indirect effect
* **A family selector** that decides which fitter to attach to each spec based
  on career tier, escalation state, and outcome shape. At low tiers it sticks
  to `lm`. As tiers rise it leans on `glm`. Under desperation it occasionally
  forces a coercion (binomial on a continuous outcome, Poisson on a rounded
  one) — the shooter chasing the next-decimal p-value.
* **Search moves**: predictor-subset enumeration, transformation grids
  (`log`, `sqrt`, `^2`, centring), interaction screening, outlier-exclusion
  seeds, subgroup splits, and a derived-metrics escalation arc when the
  budget is running out.
* **Single result contract**: every fitter returns the same shape so the
  selector, the highlight chooser, and the run record never branch on family.

### The output generators

A finished run is a `tx_run`. Five generators turn it into something
shippable, each gated to a career tier:

```r
manuscript(run)         # IMRaD draft, Methods that match the *winning* spec
presentation(run)       # 8-slide deck (slide 7 contains the only honest figure)
graphical_abstract(run) # the figure your PI will retweet
reviewer_response(run)  # opens with "we thank the reviewer for their thoughtful comments"
funding(run)            # the next grant, citing the just-shipped finding
```

Each writes to `tempdir()` by default and returns the file path invisibly.
Override with `output_dir =` or `options(texanshootR.output_dir = ...)`.

### Career, achievements, cosmetics

Every run feeds a persistent profile, because the joke does not work without
progression — and because progression *gates the API*.

```r
career()        # level, runs, favourite method, opaque scores
achievements()  # 20 unlockable badges; hidden ones show as ???
wardrobe()      # equipped cosmetic slots (hat, badge, cloak, poncho, lanyard)
progress()      # HUD: which functions are unlocked, what the next unlock requires
run_log()       # tibble of every run on this profile
```

Tiers and what they unlock:

| Career tier        | Unlocks                                              |
|--------------------|------------------------------------------------------|
| Junior Researcher  | `shoot()`, `career()`, `achievements()`, `wardrobe()`, `run_log()`, `progress()`, resets — and only the `lm` family |
| Postdoc            | `manuscript()`; `cor` and `glm` families enter the search pool |
| Senior Scientist   | `reviewer_response()`, `graphical_abstract()`, `presentation()`; `wls` and `gam` families |
| PI                 | `funding()`; `glmm` and `sem` families                |

Locked output generators do not error — they print a deadpan status block
showing what tier they need. Locked families simply never appear in the
search trace. Both unlock in place, no reinstall.

### Persistent state and saves

State persists under `tools::R_user_dir("texanshootR", "data")` as flat YAML
that you can read, edit, or version-control. The first interactive save
prompts before writing anything to disk. Opt out entirely with
`options(texanshootR.save_enabled = FALSE)` and the package becomes
pure-stateless: every call is independent, but the progression mechanic
stays inert at Junior Researcher.

### The message pack

The TUI is driven by a YAML-backed message registry under `inst/messages/`
with **1,257 entries** across phases (`blip`, `loading`, `promotion`,
`reviewer`, `derived_escalation`, `state_transition`, `banner`,
`event`/`event_consequence`). Every message carries:

* a fallacy tag from `vocab_tags` (e.g. `p_hacking`, `harking`,
  `subgroup_fishing`, `causal_overreach`),
* a rarity weight,
* an optional `mascot_state_affinity` so the right line lands at the right
  emotional register,
* an optional `model_family_affinity` so the GAM-specific blip only fires
  when the run actually picked a GAM. Every family — `lm`, `cor`, `glm`,
  `wls`, `gam`, `glmm`, `sem` — has dedicated coverage.

You can audit the pack from R:

```r
validate_messages()    # schema + tag-vocabulary check
vocab_tags             # canonical fallacy + thematic tags
vocab_phases           # canonical trigger phases
vocab_mascot_states    # composed / uncertain / anxious / desperate / resolved
vocab_careers          # tier ladder
```

The schema lives at [`MESSAGE_SCHEMA.md`](MESSAGE_SCHEMA.md). Adding a new
message is a YAML edit and a re-run of `validate_messages()`.

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

The package compiles a small C++ backend (penalised least squares for `gam`,
profile-likelihood mixed model for `glmm`) on first install. No external
solver dependencies.

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
