# texanshootR: Roguelike-Themed Framework for Exploratory Model Search

A structured, terminal-first interface for exploratory linear-model
search. The main entry point is
[`shoot()`](https://gillescolling.com/texanshootR/reference/shoot.md),
which fits a battery of candidate specifications across predictor
subsets, transformations, interactions, and outlier-removal seeds.
Persistent career state tracks run history, awards achievements, and
unlocks output generators
([`manuscript()`](https://gillescolling.com/texanshootR/reference/manuscript.md),
[`preprint()`](https://gillescolling.com/texanshootR/reference/preprint.md),
[`presentation()`](https://gillescolling.com/texanshootR/reference/presentation.md),
[`reviewer_response()`](https://gillescolling.com/texanshootR/reference/reviewer_response.md),
[`graphical_abstract()`](https://gillescolling.com/texanshootR/reference/graphical_abstract.md),
[`funding()`](https://gillescolling.com/texanshootR/reference/funding.md)).

## Persistence

On the first interactive save, the package prompts before writing to
[`tools::R_user_dir()`](https://rdrr.io/r/tools/userdir.html). Set
`options(texanshootR.save_enabled = FALSE)` to opt out entirely.

## Reproducibility

[`shoot()`](https://gillescolling.com/texanshootR/reference/shoot.md)
accepts a `seed` argument; every run records its seed, R version,
package version, and search-grid hash so past runs can be replayed.

## See also

Useful links:

- <https://gillescolling.com/texanshootR/>

- <https://github.com/gcol33/texanshootR>

- Report bugs at <https://github.com/gcol33/texanshootR/issues>

## Author

**Maintainer**: Gilles Colling <gilles.colling051@gmail.com>
([ORCID](https://orcid.org/0000-0003-3070-6066))
