# Plan: migrate to true cursed land

Replace the search's row-index-based sample manipulation with a catalog of moves
that read like principled methodological choices. A reviewer skimming the
generated abstract should nod along, not flinch.

The current package is too toy-like because the perturbations announce themselves:
`random_5pct`, `halves_a`, `leverage_top_5pct` all telegraph the dredge. The
cursed register is **plausible methods, applied for indefensible reasons** — the
shooter does what a real researcher would do under deadline pressure, and the
results sound like a real paper.

## What dies

`R/shoot_search.R:227` (`apply_outlier_seed`) — both branches.

- `random_5pct`: no human has ever written that in a paper.
- `leverage_top_5pct`: drops extreme y-values directly. Reads as outcome fishing.

`R/shoot_search.R:244` (`apply_subgroup_seed`) — both branches.

- `halves_a` / `halves_b`: row-order splits. No methodological cover.

`PERTURB_KINDS` (`R/shoot_search.R:26`) drops `outlier`, `subgroup`. Gains
`restriction`, `outcome`, `model`.

`new_spec()` (`R/shoot_search.R:28`) loses `outlier_seed` / `subgroup_seed`.
Gains `restriction` / `outcome_construction` / `model_form` (each NULL by
default).

## New catalog

Five perturbation kinds. `transform` and `interaction` are unchanged. The
other three are below.

### restriction — sample exclusion that reads like a methods choice

| Sub-kind | What it does | Trigger condition | Methods fragment |
|---|---|---|---|
| `complete_cases` | drop rows with NA in the model frame | always | "restricted to complete cases" |
| `outcome_iqr` | Tukey fence (Q1−1.5·IQR, Q3+1.5·IQR) on outcome | numeric outcome, n≥30 | "after winsorising extreme observations" |
| `predictor_iqr` | Tukey fence on one chosen predictor | numeric predictor in subset | "restricted to participants within the typical range of {var}" |
| `cooks_d` | pilot-fit, drop top-5% Cook's D, refit | n≥30, lm-family | "excluding observations with disproportionate leverage" |
| `factor_level` | restrict to one level of a real factor column | factor column with ≥2 levels and ≥10 rows per level | "in the {level} subgroup of {factor}" |

### outcome — outcome engineering

| Sub-kind | What it does | Trigger condition | Methods fragment |
|---|---|---|---|
| `composite_index` | scale + row-mean of 2–4 numeric cols not in subset → new outcome | ≥2 numeric cols outside subset | "we constructed a composite index from {vars}" |
| `residualise` | y ← residuals of `y ~ z` for one numeric covariate not in subset | numeric covariate available | "after partialling out variation due to {z}" |
| `dichotomise` | split y at median, swap family to quasibinomial | numeric outcome, n≥30 | "comparing high- vs low-{y} groups" |
| `ratio` | y ← y / x_baseline for one strictly-positive predictor | strictly positive numeric available | "expressed as a rate per unit of {baseline}" |
| `z_within` | z-score y within each level of a factor | factor column exists | "z-scored within each {factor}" |

### model — model-complexity escalation

| Sub-kind | What it does | Trigger condition | Methods fragment |
|---|---|---|---|
| `polynomial` | wrap a predictor as `poly(x, 2)` or `poly(x, 3)` | numeric predictor in subset | "allowing for a quadratic trend in {x}" |
| `spline` | wrap as `splines::ns(x, df = 3)` | numeric predictor in subset, n≥30 | "modelling {x} flexibly via natural splines" |
| `robust` | swap fitter to `MASS::rlm` | numeric outcome | "using a robust M-estimator" |
| `family_swap` | swap to `glm(family ∈ {poisson, Gamma(log), quasibinomial})` | family plausible for outcome | "modelled with a {family} GLM" |
| `random_effect` | swap to `lme4::lmer` with `(1 | group)` | factor column with ≥3 levels | "with a random intercept for {group}" |

## The payoff: chained methods sentence

Each spec can hold at most one entry per kind. Stacks render as a single
chained sentence in the abstract / manuscript. A spec with
`restriction = complete_cases`, `outcome = z_within(sex)`,
`model_form = robust` produces:

> *After restricting to complete cases and z-scoring weight within each sex, we
> fit a robust M-estimator of weight on height with a quadratic age term
> (n = 218, R² = 0.34, p = .031).*

That sentence is the curse. It would pass a methods review. It is exactly what
a tired postdoc submits at 2am. Nothing in it references row numbers, random
exclusions, or arbitrary halves.

## Spec schema (before / after)

Before (`R/shoot_search.R:28`):

```r
list(subset, transforms, interactions,
     outlier_seed, subgroup_seed, source, family)
```

After:

```r
list(subset, transforms, interactions,
     restriction          = NULL,   # list(kind, ...) or NULL
     outcome_construction = NULL,   # list(kind, ...) or NULL
     model_form           = NULL,   # list(kind, ...) or NULL
     source, family)
```

`subset_key()` (`R/shoot_search.R:47`) is unchanged: it keys the search-state
hash on the predictor subset only, and the three new axes attach as separate
state on the spec.

## Mechanic-to-fallacy contract (new)

Rewrite `R/shoot_search.R:14-19`:

```
subsets       = predictor dredging / multiple comparisons
transforms    = researcher degrees of freedom
interactions  = p-hacking
restrictions  = principled-sounding sample exclusion
outcome_con   = outcome engineering / construct-irrelevant composites
model_form    = model-complexity escalation
```

## File-touch list

### Phase 1 — R-only, no engine work (restriction + outcome_construction)

- `R/shoot_search.R` — rewrite top contract block (lines 14-19), `PERTURB_KINDS`
  (line 26), `new_spec` (line 28), `perturb_spec` (line 70), `materialise_spec`
  (line 197). Replace `apply_outlier_seed` and `apply_subgroup_seed` with
  `apply_restriction` and `apply_outcome_construction`. Each sub-kind branch
  checks its trigger condition before applying.
- `R/output_helpers.R` — add `describe_spec(spec, run)` returning the chained
  methods fragment. Single source of truth used by every output generator.
- `R/output_abstract.R:34` — pipe `describe_spec()` into `build_abstract_text()`.
  Replace the current generic body.
- `R/output_manuscript.R`, `R/output_presentation.R`, `R/output_reviewer_response.R`,
  `R/output_graphical_abstract.R`, `R/output_funding.R` — each consumes
  `describe_spec()` so the cursed sentence is consistent across the chain.
- `R/modifiers_live.R` — `bias_perturb` tag set updates. `+outliers` →
  biases `restriction` (Cook's D / Tukey sub-kinds). `+subgroups` → biases
  `restriction` (factor_level). New tags TBD for `outcome` and `model` biases.
- `R/shoot_derived.R` — desperation-phase enqueue uses the new kinds.
- `inst/messages/loading_general.yaml` — sweep for "outlier", "subgroup",
  "halves", "random". Replace with lines that name the cursed moves
  ("dropping the pilot wave", "winsorising", "controlling for…").
- `tests/testthat/*.R` — sweep references to `outlier_seed`, `subgroup_seed`,
  `halves_a`, `random_5pct`, `leverage_top_5pct`. Add unit tests for each new
  sub-kind covering: trigger condition (no-op when context absent), apply
  correctness, and `describe_spec()` rendering.

### Phase 2 — engine work (model_form)

The engine already supports per-spec family fallback: `R/shoot_engine.R:36-41`
routes non-lm specs through `fit_spec()` one at a time. So model_form adds
plug into `families.R` without C++ changes.

- `R/families.R` / `R/family_select.R` — register `rlm`, `lmer`,
  `glm_poisson`, `glm_gamma_log`, `glm_quasibinomial` fitters. Each returns
  the same tidy result row shape as the existing `lm` / `glm` fitters.
  `model_form` sub-kind on the spec overrides the auto-picked family.
- `R/shoot_engine.R` — confirm batch path still routes correctly when
  `family$fitter` is set by `model_form` rather than by `family_select`.
- `DESCRIPTION` Imports — add `MASS`, `lme4`. `splines` is base, no add
  needed. Per CLAUDE.md these are both core CRAN; no Suggests + fallback
  dance needed.

### Phase 3 — voice polish

- Mascot blip-stream and modifier glyphs are off-limits per the
  "never change the look" voice rule. Only label / message text updates.
- NEWS.md entry under a `0.2.0 (planned)` heading. DESCRIPTION Version
  stays at `0.1.0` per the pre-release versioning rule.

## Order of work

Phase 1 alone gets ~80% of the cursed-believable feeling and lands without
engine changes. Recommended sequence:

1. Land the schema change (`new_spec` shape + `subset_key` unchanged).
2. Wire `restriction` sub-kinds with trigger checks. Existing test suite
   stays green by aliasing `outlier_seed` → `restriction = complete_cases`
   for the transition commit, then deleting the alias once tests are rewritten.
3. Wire `outcome_construction` sub-kinds.
4. Build `describe_spec()` and route the abstract through it. Smoke-test
   the chain by running `shoot()` and reading the abstract.
5. Rewrite tests.
6. Phase 2: model_form fitters.
7. Phase 3: messages, NEWS.

## Open questions before I touch code

1. **Cook's D peeks at the outcome.** It's the most cursed move in the
   catalog because it's a real, reviewer-acceptable, publication-grade form
   of cheating (drop influential points based on the residual you just fit).
   Keep it? Pull it?
2. **Stacking depth.** One sub-kind per kind, or allow e.g. two restrictions
   chained (`complete_cases` + `factor_level`)? Stacking is more cursed and
   produces longer methods sentences; single is simpler to test.
3. **Composite-index source columns.** Pull from numeric columns *not* in
   the current predictor subset (cleaner read: "constructed from other
   measured variables"), or any numeric column in `df` minus the outcome?
   Former is more believable; latter offers more variety.
4. **lme4 and MASS as hard deps.** Phase 2 imports both. Both are common
   core-CRAN packages. OK, or prefer Suggests + skip those sub-kinds when
   unavailable?
5. **Engine slowdown.** glm / rlm / lmer cost 3–30× a fast `.lm.fit`. The
   wall-clock budget naturally throttles them, but the search may rarely
   pick model_form specs because they reduce iteration count. Want a
   budget-aware bias that boosts model_form when budget is generous, or
   leave it to natural attrition?
6. **`bias_perturb` tag names.** Current tactical modifiers are `+outliers`
   and `+subgroups`. After the migration these still map cleanly to
   sub-kinds of `restriction`. Want new tags for the two new axes
   (e.g. `+inventive` for outcome construction, `+heroic` for model form),
   or fold them into the existing modifier vocabulary?
