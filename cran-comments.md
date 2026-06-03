## Submission

Patch release (0.1.0 -> 0.1.1) addressing the CRAN MKL builder
"Additional issues" failure flagged in the maintainer email of
2026-06-03 ("Please correct before 2026-06-17").

## What changed

* `shoot()`: the search loop now runs the fit phase to the seed-and-
  budget-derived spec cap regardless of wall-clock, then enters the
  animate-out phase. Previously the loop predicate (`Sys.time() <
  end_time`) could truncate the fit phase on a slow host, producing
  different trace lengths -- and therefore different `grid_hash`
  values -- for two same-seed runs. This caused
  `test-shoot.R:32:3` ("seed makes runs reproducible") to fail on the
  CRAN MKL Fedora Clang builder (`100-26654af9` vs `80-46c81095`).
  No user-visible API change; runs on slow hosts may now slightly
  exceed the wall-clock budget to keep the documented "deterministic
  in seed + budget" guarantee.

## Test environments

* Local: Windows 11 x64, R 4.6.0 (release) -- `R CMD check --as-cran`, 0 NOTEs
* win-builder: R-devel
* win-builder: R-release

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

None.
