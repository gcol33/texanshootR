## Submission

First submission of texanshootR to CRAN.

## Test environments

* Local: Windows 11 x64, R 4.6.0 (release) -- `R CMD check --as-cran`, 1 NOTE (New submission)
* win-builder: R-devel -- 1 NOTE (New submission)
* win-builder: R-release (R 4.6.0) -- 1 NOTE (New submission)
* win-builder: R-oldrelease (R 4.5.3) -- 1 NOTE (New submission; see below)

## R CMD check results

0 errors | 0 warnings | 1 note

The single NOTE is the expected "New submission" flag for a first CRAN
upload.

On the R-oldrelease win-builder run, the URL check additionally returned
a transient HTTP 429 ("Too Many Requests") from
`https://en.wikipedia.org/wiki/Texas_sharpshooter_fallacy` (the
package's namesake article). The URL is valid; subsequent runs and the
R-devel / R-release checks returned 200.

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
