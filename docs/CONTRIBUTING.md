# Contributing to texanshootR

Most contributions to texanshootR are *content*, not code: new messages,
new life events, new achievements, new cosmetics. The package is built
so that you can extend the corpus by dropping YAML into `inst/` without
touching R source.

## Adding messages

1.  Read `MESSAGE_SCHEMA.md` end-to-end. The schema is the contract.

2.  Pick a category file under `inst/messages/` or create a new one. One
    file per category keeps PR diffs readable.

3.  Every entry must carry at least one **fallacy tag** when it lives in
    a `loading`, `blip`, `derived_escalation`, `ultra_rare`, or `event`
    phase. Phases like `banner`, `promotion`, `reviewer`, `daily`, and
    `state_transition` are exempt — they’re institutional ceremony, not
    methodological mechanics.

4.  Tone: **deadpan everywhere**. The package never explains the joke
    inside its own output. No “lol”, no winks, no scare quotes, no moral
    warnings. Reread your draft and reject anything that reads as a
    meme.

5.  Run the validator:

    ``` r

    devtools::load_all()
    texanshootR::validate_messages()
    ```

## Adding life events

Schema and field reference live in `MESSAGE_SCHEMA.md` under “Event
schema”. Effects must use keys from `vocab_effects` and stay within
`[-0.5, +0.5]`. The validator caps these on load.

## Adding achievements

`inst/achievements/v1.yaml` is a single bundled file in v1. Each entry
needs a `trigger` function defined in `R/achievements.R`. Keep triggers
pure (run + meta in, logical out) and side-effect-free.

Visible achievements describe progression. Hidden achievements describe
discovery — `name: "???"` and `hint: ~` until unlocked.

## Adding cosmetics

`inst/cosmetics/v1.yaml`. Each entry declares a slot and a small ASCII
overlay. The overlay must not change layout width by more than a few
characters or it desyncs the mascot frame.

## Code style

The R source follows these rules:

- Two-space indent, no tabs.
- Snake_case for everything.
- Avoid heavy dependencies. Core deps are limited to `cli`, `yaml`,
  `jsonlite`, `withr`, and base packages. Output functions gate on
  [`requireNamespace()`](https://rdrr.io/r/base/ns-load.html).
- Never write outside
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html) from examples or
  tests. The consent-gated save dir is the single place persistent state
  lives.

## Testing

``` r
devtools::test()
R CMD check --as-cran
```

Tests must be hermetic — they run with a temp save dir, animations off,
life events disabled, quiet output. See
`tests/testthat/helper-isolated.R`.

## Pull requests

- Keep PRs small. One category file or one feature per PR is ideal.
- Run
  [`validate_messages()`](https://gillescolling.com/texanshootR/reference/validate_messages.md)
  and
  [`devtools::test()`](https://devtools.r-lib.org/reference/test.html)
  before opening.
- Don’t introduce new vocab_phases, vocab_tags, or vocab_effects values
  without flagging the change in the PR description.
