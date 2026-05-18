## Submission

First submission of texanshootR to CRAN.

## Test environments

* Local: Windows 11 x64, R 4.6.0 (release) -- `R CMD check --as-cran`, Status: OK
* win-builder: R-devel -- pending
* win-builder: R-release -- pending
* R-hub: ubuntu-latest, macos-latest, windows-latest -- pending

## R CMD check results

0 errors | 0 warnings | 0 notes

## Notes for the reviewer

* texanshootR is a didactic / parody package. The Title
  ("Reproducible Audit Trails for Indefensible Research") and parts of
  the Description ("principled-sounding sample restrictions", "outcome
  engineering", "model-form escalation") use deliberate satirical
  framing to characterise the questionable-research-practice taxonomy
  the package encodes. The functionality itself is real: a budgeted
  exploratory linear-model search across transformations, predictor
  subsets, interactions, sample restrictions, outcome constructions,
  and model-form lifts, with a terminal UI and a six-stage output
  pipeline. Happy to adjust wording if the satirical framing is
  unacceptable.

* All persistent state is written under `tools::R_user_dir(...,
  "data")` and only after the user sets `options(texanshootR.consent =
  TRUE)`. Without consent, save I/O is restricted to `tempdir()`.
  Default `output_dir` for generated artefacts is `tempdir()`.

* No examples or tests write outside `tempdir()`. No examples or tests
  modify the user's filespace, options, environment variables, or
  working directory beyond `withr::local_*` scoping.

* The package vendors a YAML message bank under `inst/messages/`
  (~200 KB). Installed package size is well under 5 MB.

## Reverse dependencies

None (new submission).
