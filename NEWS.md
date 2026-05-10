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
* `manuscript()`, `preprint()`, `presentation()`, `reviewer_response()`, `graphical_abstract()`, `funding()`.
* Default output dir is `tempdir()`; override with `output_dir =` or `options(texanshootR.output_dir = ...)`.

## Reset and configuration
* `reset_career()`, `reset_achievements()`, `reset_wardrobe()`, `reset_all()`.
* Package options documented at `?\`texanshootR-options\``.

## Internals
* `validate_messages()`: schema check for the bundled YAML message packs.
* `vocab_*()` family: enumerates the vocabularies that drive message selection and effect application.
