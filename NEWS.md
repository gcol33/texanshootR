# texanshootR 0.1.0

Initial release.

## Search engine

* `shoot()`: budgeted exploratory linear-model search across predictor
  subsets, transformations, interactions, sample restrictions, outcome
  constructions, and model-form lifts.
* `restriction` axis: complete-cases pruning, Tukey IQR fence on the
  outcome or a predictor, Cook's D leverage exclusion, and factor-level
  restriction.
* `outcome_construction` axis: composite indices, residualisation,
  ratio outcomes, within-group z-scoring.
* `model_form` axis: polynomial / spline predictor wraps, native Huber
  M-estimator (`rlm`), GLM family swaps, random-intercept lifts.
* Derived-metric escalation when no spec reaches p &le; 0.05.
* Methods sentence rendered consistently across `print(run)` and every
  output generator via a shared `describe_spec()` helper.
* `summary(run)` reports counts for each axis.
* `run_log()`: persisted history of recorded runs.
* Reproducibility: every run records its seed, R version, package
  version, and a hash of the search grid.

## Live modifiers

The cursed bestiary covers questionable-research-practice tactics that
fire as in-run tactical-pause modifiers:

* `+aggregate` (alias `+agg`): collapse records to country / group means
  before fit. Hides within-group variance when within-group variance
  exceeds between-group variance.
* `+compilation` (alias `+comp`): treat a literature compilation of
  reported values as ground-truth response. Reporting bias gets
  re-emitted as ecological signal.
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
  want to interpret it. Variance reassigns to the predictors of interest
  and strengthens their apparent effects.
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
  dismiss a statistical concern by appealing to domain mechanism rather
  than engaging with the statistical content. The null simulation is
  set aside because the mechanism is "obvious".

### Deflection family

Five modifiers covering distinct rhetorical moves that redirect
attention away from a methodological concern rather than engaging
with it. All share the `deflection` fallacy tag.

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

* `vocab_tags` includes `aggregation_artifact` and `deflection`
  fallacy tags.

## Output generators

Career-gated output set: locked functions return a status block and
become usable once the tier is reached. Unlocks persist across
sessions via the save state.

* `manuscript()` (Postdoc).
* `presentation()`, `reviewer_response()`, `graphical_abstract()`
  (Senior Scientist).
* `funding()` (PI).
* Default output dir is `tempdir()`; override with `output_dir =` or
  `options(texanshootR.output_dir = ...)`.

## Persistent career

* `career()`: dashboard of level, runs, favorite method, opaque
  qualitative scores.
* `achievements()`: registry of visible and hidden achievements with
  unlock status.
* `wardrobe()`: ASCII cosmetic slots that auto-equip when their
  unlocking achievement fires.
* `progress()`: HUD-style companion to `?<fn>`. Reads live save state
  and prints which functions are unlocked, which are locked, and what
  the next unlock requires.
* Consent-gated save dir under `tools::R_user_dir("texanshootR",
  "data")`.

## Reset and configuration

* `reset_career()`, `reset_achievements()`, `reset_wardrobe()`,
  `reset_all()`.
* Package options documented at `?\`texanshootR-options\``.

## Internals

* `require_unlocked()`: single source of truth for output-tier gating.
  Signals a `tx_locked` condition so scripts fail loudly while
  interactive callers can `tryCatch(tx_locked = ...)`. Roxygen
  `@description` on each gated function carries the static unlock
  requirement so `?presentation` documents the tier even when the
  function is callable.
* `validate_messages()`: schema check for the bundled YAML message
  packs.
* `vocab_*()` family: enumerates the vocabularies that drive message
  selection and effect application.
