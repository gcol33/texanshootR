# texanshootR 0.2.0 (planned)

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
