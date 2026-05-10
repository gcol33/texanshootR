# Changelog

## texanshootR 0.2.0 (planned)

### API unlocks

The output generators are gated by career tier. Calling a locked
function returns a deadpan status block; the function is otherwise
usable once the tier is reached. Unlocks are persistent across sessions
via the existing save state.

- Junior Researcher:
  [`shoot()`](https://gillescolling.com/texanshootR/reference/shoot.md),
  [`career()`](https://gillescolling.com/texanshootR/reference/career.md),
  [`achievements()`](https://gillescolling.com/texanshootR/reference/achievements.md),
  [`wardrobe()`](https://gillescolling.com/texanshootR/reference/wardrobe.md),
  [`run_log()`](https://gillescolling.com/texanshootR/reference/run_log.md),
  [`progress()`](https://gillescolling.com/texanshootR/reference/progress.md),
  resets.

- Postdoc:
  [`manuscript()`](https://gillescolling.com/texanshootR/reference/manuscript.md),
  [`preprint()`](https://gillescolling.com/texanshootR/reference/preprint.md).

- Senior Scientist:
  [`reviewer_response()`](https://gillescolling.com/texanshootR/reference/reviewer_response.md),
  [`graphical_abstract()`](https://gillescolling.com/texanshootR/reference/graphical_abstract.md),
  [`presentation()`](https://gillescolling.com/texanshootR/reference/presentation.md).

- PI:
  [`funding()`](https://gillescolling.com/texanshootR/reference/funding.md).

- [`progress()`](https://gillescolling.com/texanshootR/reference/progress.md):
  HUD-style companion to `?<fn>`. Reads live save state and prints which
  functions are unlocked, which are locked, and what the next unlock
  requires.

- `require_unlocked()` (internal): single source of truth for gating.
  Signals a `tx_locked` condition so scripts fail loudly while
  interactive callers can `tryCatch(tx_locked = ...)`.

- Roxygen `@description` on each gated function carries the static
  unlock requirement, so
  [`?presentation`](https://gillescolling.com/texanshootR/reference/presentation.md)
  documents the tier even when the function is callable.

## texanshootR 0.1.0

Initial release.

### Search engine

- [`shoot()`](https://gillescolling.com/texanshootR/reference/shoot.md):
  exploratory linear-model search across predictor subsets,
  transformations, interactions, outlier-removal seeds, and subgroup
  seeds.
- [`run_log()`](https://gillescolling.com/texanshootR/reference/run_log.md):
  returns the persisted history of recorded runs.
- Derived-metric escalation when no spec reaches p ≤ 0.05.
- Reproducibility: every run records its seed, R version, package
  version, and a hash of the search grid.

### Persistent career

- [`career()`](https://gillescolling.com/texanshootR/reference/career.md):
  dashboard of level, runs, favorite method, opaque qualitative scores.
- [`achievements()`](https://gillescolling.com/texanshootR/reference/achievements.md):
  registry of visible and hidden achievements with unlock status.
- [`wardrobe()`](https://gillescolling.com/texanshootR/reference/wardrobe.md):
  ASCII cosmetic slots that auto-equip when their unlocking achievement
  fires.
- Consent-gated save dir under
  `tools::R_user_dir("texanshootR", "data")`.

### Output generators

- [`manuscript()`](https://gillescolling.com/texanshootR/reference/manuscript.md),
  [`preprint()`](https://gillescolling.com/texanshootR/reference/preprint.md),
  [`presentation()`](https://gillescolling.com/texanshootR/reference/presentation.md),
  [`reviewer_response()`](https://gillescolling.com/texanshootR/reference/reviewer_response.md),
  [`graphical_abstract()`](https://gillescolling.com/texanshootR/reference/graphical_abstract.md),
  [`funding()`](https://gillescolling.com/texanshootR/reference/funding.md).
- Default output dir is
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html); override with
  `output_dir =` or `options(texanshootR.output_dir = ...)`.

### Reset and configuration

- [`reset_career()`](https://gillescolling.com/texanshootR/reference/reset.md),
  [`reset_achievements()`](https://gillescolling.com/texanshootR/reference/reset.md),
  [`reset_wardrobe()`](https://gillescolling.com/texanshootR/reference/reset.md),
  [`reset_all()`](https://gillescolling.com/texanshootR/reference/reset.md).
- Package options documented at `?\`texanshootR-options\`\`.

### Internals

- [`validate_messages()`](https://gillescolling.com/texanshootR/reference/validate_messages.md):
  schema check for the bundled YAML message packs.
- `vocab_*()` family: enumerates the vocabularies that drive message
  selection and effect application.
