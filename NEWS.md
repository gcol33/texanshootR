# texanshootR 0.2.0 (planned)

## Search engine

* New `restriction` axis replaces row-index-based outlier / subgroup
  seeds. Sub-kinds: complete-cases pruning, Tukey IQR fence on the
  outcome or a predictor, Cook's D leverage exclusion, and factor-level
  restriction.
* New `outcome_construction` axis: composite indices, residualisation,
  ratio outcomes, within-group z-scoring.
* New `model_form` axis: polynomial / spline predictor wraps, native
  Huber M-estimator (`rlm`), GLM family swaps, random-intercept lifts.
* Methods sentence rendered consistently across `print(run)`, the
  abstract / manuscript / presentation / reviewer / graphical / funding
  generators via a shared `describe_spec()` helper. The cursed sentence
  reads as a normal methods paragraph.
* `summary(run)` reports counts for each axis.

## Live modifiers — new sins

Five new modifiers expanding the cursed bestiary:

* `+aggregate` (alias `+agg`): collapse records to country / group means
  before fit. Hides within-group variance when within-group variance
  exceeds between-group variance.
* `+compilation` (alias `+comp`): treat a literature
  compilation of reported values as ground-truth response. Reporting
  bias gets re-emitted as ecological signal.
* `+gapfill` (alias `+gap`): validate predictions for unobserved cases
  using fit metrics on the observed cases. The reported metric is silent
  precisely where the question lives.
* `+tier2` (alias `+relax`, `+downweight`): when the pre-registered
  quality filter yields too few cases, introduce a downweighted second
  tier and backdate the threshold to Methods.
* `+counter_argument` (alias `+counter`, `+preempt`): inscribe the
  reviewer's likely objections and their rebuttals into the manuscript.
  Closes off review by performing it on the reviewer's behalf.
* `+pseudo_spatial` (alias `+coords`): add geographic coordinates as
  fixed effects and treat that as having handled spatial
  autocorrelation. Residuals still cluster.
* `+omit_control` (alias `+omit`): drop a covariate because you don't
  want to interpret it. Variance reassigns to the predictors of
  interest and strengthens their apparent effects.
* `+knife_edge` (alias `+labeling`): define group membership by a
  knife-edge presence rule on a continuous variable. The comparison
  effect emerges from the labelling rule, not the underlying process.
* `+iid_rescue` (alias `+iid`): reassign an inconvenient fixed effect
  to an iid random effect when the result is otherwise unwelcome.
* `+defer_fix` (alias `+defer`, `+next_step`): acknowledge a
  methodological correction; defer it to the next paper / first step /
  future analysis while the broken analysis proceeds unchanged.
* `+coffee_break` (alias `+side_meeting`, `+offline`): move
  methodological disagreement off the written record. A six-paragraph
  objection is answered with a one-line proposal to "discuss it
  informally" at the retreat.
* `+mechanism_rebuttal` (alias `+intuition`, `+but_biologically`):
  dismiss a statistical concern by appealing to domain mechanism
  rather than engaging with the statistical content. The null
  simulation is set aside because the mechanism is "obvious".

### Deflection family

Five modifiers covering distinct rhetorical moves that redirect
attention away from a methodological concern rather than engaging
with it. All share the new `deflection` fallacy tag.

* `+misunderstanding` (alias `+you_misread`): relocate the
  methodological problem from the analysis to the reader.
* `+narrow_answer` (alias `+strawman`): answer a weaker version of
  the concern at the wrong level of analysis.
* `+complexity_shield` (alias `+its_complex`): invoke unspecified
  complexity to dismiss a specific narrow concern.
* `+frame_shift` (alias `+oscillate`): oscillate between descriptive
  and causal framing depending on which is being challenged.
* `+matter_of_taste` (alias `+just_one_way`): soften a falsifiable
  concern into a matter of perspective.

* `vocab_tags` gains `aggregation_artifact` and `deflection`
  (fallacy tags).
* New message banks: `aggregation_artifact.yaml`,
  `compilation_as_truth.yaml`, `gapfill_validation.yaml`,
  `threshold_relax.yaml`, `reviewer_prophecy.yaml`,
  `pseudo_spatial.yaml`, `omit_control.yaml`, `knife_edge.yaml`,
  `iid_rescue.yaml`, `defer_fix.yaml`, `coffee_break.yaml`,
  `mechanism_rebuttal.yaml`, `misunderstanding.yaml`,
  `narrow_answer.yaml`, `complexity_shield.yaml`, `frame_shift.yaml`,
  `matter_of_taste.yaml`. Yaml headers document the cursed-practice
  language tells observed in field-report sightings.

## Breaking changes

* `preprint()` removed. The canonical output set is `manuscript()`,
  `presentation()`, `graphical_abstract()`, `reviewer_response()`,
  `funding()`. Manuscripts already cover the preprint use case.

## API unlocks

The output generators are gated by career tier. Calling a locked function
returns a status block; the function is otherwise usable once the tier is
reached. Unlocks are persistent across sessions via the existing save
state.

* Junior Researcher: `shoot()`, `career()`, `achievements()`, `wardrobe()`,
  `run_log()`, `progress()`, resets.
* Postdoc: `manuscript()`.
* Senior Scientist: `reviewer_response()`, `graphical_abstract()`,
  `presentation()`.
* PI: `funding()`.

* `progress()`: HUD-style companion to `?<fn>`. Reads live save state and
  prints which functions are unlocked, which are locked, and what the next
  unlock requires.
* `require_unlocked()` (internal): single source of truth for gating.
  Signals a `tx_locked` condition so scripts fail loudly while interactive
  callers can `tryCatch(tx_locked = ...)`.
* Roxygen `@description` on each gated function carries the static unlock
  requirement, so `?presentation` documents the tier even when the function
  is callable.

# texanshootR 0.1.0

Initial release.

## Search engine
* `shoot()`: exploratory linear-model search across predictor subsets, transformations, interactions, outlier-removal seeds, and subgroup seeds.
* `run_log()`: returns the persisted history of recorded runs.
* Derived-metric escalation when no spec reaches p &le; 0.05.
* Reproducibility: every run records its seed, R version, package version, and a hash of the search grid.

## Persistent career
* `career()`: dashboard of level, runs, favorite method, opaque qualitative scores.
* `achievements()`: registry of visible and hidden achievements with unlock status.
* `wardrobe()`: ASCII cosmetic slots that auto-equip when their unlocking achievement fires.
* Consent-gated save dir under `tools::R_user_dir("texanshootR", "data")`.

## Output generators
* `manuscript()`, `presentation()`, `reviewer_response()`, `graphical_abstract()`, `funding()`.
* Default output dir is `tempdir()`; override with `output_dir =` or `options(texanshootR.output_dir = ...)`.

## Reset and configuration
* `reset_career()`, `reset_achievements()`, `reset_wardrobe()`, `reset_all()`.
* Package options documented at `?\`texanshootR-options\``.

## Internals
* `validate_messages()`: schema check for the bundled YAML message packs.
* `vocab_*()` family: enumerates the vocabularies that drive message selection and effect application.
