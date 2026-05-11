# texanshootR

Terminal-first parody package for exploratory linear-model search. `shoot(df)`
opens a wall-clock-budgeted search across spec perturbations (transformations,
predictor subsets, interactions, outlier seeds), renders a multi-zone TUI with
a mascot reacting to progress, and surfaces the highlighted result as a
"finding". Progression unlocks a six-stage publication chain that gates the
output generators.

## Voice & design rules (read before editing user-facing text)

These come from prior sessions. They are load-bearing — do not break them in
the name of a "fix".

- **Never change the visual look as a side-effect of a bug fix.** Width /
  alignment / jitter bugs get fixed at the *measurement* layer (cell widths,
  padding math, layout calculations). Do **not** swap glyphs, mascot ASCII, gun
  ASCII, fonts, or color tokens. If the fix genuinely requires a look change,
  ask first with a concrete preview.
- **User voice beats anti-AI rules.** The README and message banks are in a
  deliberate gunslinger / mock-formal / academic-deadpan register. Phrases that
  pattern-match to AI tells ("In this day and age...", "publish or perish
  reigns king...") are voice, not slop. If an anti-AI pattern keeps flagging
  the same intentional phrase, update the skill — do not edit the writing.
- **Progression architecture, not progression flavor.** Career/achievements
  gate real API surface (the six output generators), not just cosmetics.
  Locked functions throw `stop()` with class `tx_locked` and a deadpan
  two-line status block. No second-person, no encouragement, no exclamation
  marks. Format:
  ```
  presentation() requires:
  Senior Scientist

  Current career:
  Postdoc
  ```
- **Pre-release: don't bump `DESCRIPTION` Version on feature commits.** Stay
  at the current version (0.1.0). Only bump on explicit release boundaries
  ("cut a release", "tag X.Y.Z"). NEWS.md can stage release notes under a
  "0.2.0 (planned)" header without touching DESCRIPTION.

## Architecture map

Entry point: `shoot()` in `R/shoot.R`. Everything else either feeds it or
runs off the run object it returns.

- `R/shoot.R` — main loop: budget, spec cap, mini-batched fits, mascot
  transitions, output-flag dispatch.
- `R/shoot_search.R` — spec queue / perturbation seeding / bias resolution.
- `R/shoot_engine.R` — fit_specs_batch C++ bridge.
- `R/shoot_derived.R` — desperation-phase derived metrics.
- `R/families.R` + `R/family_select.R` — model family pickers per outcome type.
- `R/ui.R` — TUI session manager. Three rendering modes (see below).
- `R/mascot.R` — emotional state ladder, heartbeat frames, cell-width tables.
- `R/modifiers_live.R` — pre-roll vs interactive tactical-pause modifiers.
- `R/chain.R` — publication chain state machine (XP, length unlocked,
  active chain, window expiry).
- `R/output_*.R` — six generators (`abstract`, `manuscript`,
  `presentation`, `reviewer_response`, `graphical_abstract`, `funding`),
  each gated on chain stage.
- `R/output_helpers.R` — `OUTPUT_GENERATORS` registry +
  `generate_flagged_outputs()` for auto-generation from `shoot()` flags.
- `R/career.R` + `R/unlocks.R` + `R/achievements.R` + `R/wardrobe.R` —
  progression / cosmetic surface.
- `R/run.R` — `print.tx_run()` narrative banner.
- `R/terminal_spawn.R` — Windows Terminal / cmd.exe spawn for `terminal=TRUE`.
- `R/messages.R` + `R/message_select.R` — message bank loader + validator
  (lints loading-phase text against `DYN_LOADING_BUDGET`).
- `R/save_io.R` + `R/save_migrate.R` — on-disk meta (career, XP, achievements,
  active chain) under `texanshootR.save_dir`.
- `inst/messages/*.yaml` — registry files. `loading_general.yaml` is the
  big one (~140KB).

## TUI rendering modes

`R/ui.R` picks one of three modes at `ui_session_open()` time:

1. **ANSI multi-zone** — real terminals (Windows Terminal, MINGW64). Five
   zones with cursor positioning: shooter / blip stream / modifiers /
   outputs / bar. `ansi_supported()` gates this.
2. **Dynamic single-line** — RStudio Console and any host that honors `\r`
   but not cursor positioning. Layout: `[bar pct] | face+gun | +mods... |
   loading-text`. Composed by `dyn_compose()`, rendered in place by
   `dyn_render()`. Blip stream and full ASCII art suppressed (would scroll).
3. **Plain** — CI, RMarkdown, anything without `\r`. Sequential lines.

User override: `options(texanshootR.ui_mode = "ansi" | "dynamic" | "plain")`.
Diagnostic: `texanshootR:::ui_mode_diagnostic()`.

RStudio quirk: the cell-width of CJK / box-drawing / Devanagari glyphs in
RStudio's default font diverges from `nchar(type = "width")`. `R/mascot.R`
has hand-tabulated `FACE_CELL_WIDTH` and a two-frame `heartbeat_frame_dyn()`
that keeps the wide-char prefix identical across ticks to stop the bar
column from jittering. Do not "fix" this by switching to ASCII glyphs.

## Publication chain

State lives in `meta$progression` (XP, length unlocked) and `meta$active_chain`
on disk. `CHAIN_STAGES` in `R/chain.R` is the canonical order:

```
abstract -> manuscript -> presentation -> reviewer_response
  -> graphical_abstract -> funding
```

XP thresholds: 0 / 5 / 15 / 30 / 55 / 90. A shippable run opens a chain;
each stage must be claimed in order within `texanshootR.chain_window`
(default 30s). Out-of-order or expired chain raises `tx_chain_error`.

`shoot(..., abstract = TRUE, manuscript = TRUE, ...)` auto-generates the
chain prefix up to the highest enabled flag via
`generate_flagged_outputs()`. Aliases: `powerpoint` = `presentation`,
`reviewer` = `reviewer_response`, `graphical` = `graphical_abstract`.

## Options

All read at point of use. Defaults in `R/options.R`.

| Option | Default | Purpose |
|---|---|---|
| `texanshootR.animations` | `TRUE` | Master switch for TUI animations |
| `texanshootR.ui_mode` | unset | Force `"ansi"` / `"dynamic"` / `"plain"` |
| `texanshootR.budget` | 30 (sec) | Wall-clock budget per `shoot()` |
| `texanshootR.chain_window` | 30 (sec) | Per-stage chain deadline |
| `texanshootR.output_dir` | `tempdir()` | Where generators write files |
| `texanshootR.save_dir` | platform dflt | Persistent meta location |
| `texanshootR.save_enabled` | `TRUE` | Disable to run entirely in memory |
| `texanshootR.consent` | unset | Required for save I/O outside tempdir |
| `texanshootR.quiet` | `FALSE` | Suppress generator status prints |
| `texanshootR.life_events` | `TRUE` | Enable life-event encounters |
| `texanshootR.event_rate` | `1/6` | Per-run life-event probability |

## Testing

```r
devtools::test()                       # full suite
devtools::test(filter = "animation")   # one file
```

`tests/testthat/helper-isolated.R` gates the option clobber on stack
inspection — `devtools::load_all()` outside testthat will not silently
disable animations any more.

The dynamic-mode end-to-end test (`tests/testthat/test-animation.R`)
patches `texanshootR:::is_interactive_ui` (not `base::interactive`,
which has a locked binding) to spoof a UI session under Rscript. Use
the same pattern when adding TUI tests.

## Build & check (Windows)

```powershell
# Always use Windows R, not WSL R (WSL R lacks devtools/roxygen2):
$RSCRIPT = (Get-ChildItem 'C:\Program Files\R\R-*\bin\Rscript.exe' |
            Sort-Object Name | Select-Object -Last 1).FullName

# Rcpp regen + docs (after editing any C++ or @export):
& $RSCRIPT -e "Rcpp::compileAttributes(); devtools::document()"

# R CMD check:
& $RSCRIPT -e "devtools::check()"
```

If you add `//'` doc comments to non-exported C++ functions you will
generate orphan `.Rd` files that R CMD check warns on — only document
C++ functions that are `@export`ed.

## Smoke scripts

Top-level dev scratch files (tracked in repo, kept for iteration):

- `dev_smoke_dyn.R` — direct dynamic-mode UI manager probe.
- `dev_modifiers_smoke.R` — modifier roll / tactical-pause probe.

Run with `Rscript`.

## Things that have bitten us

- `out[[i]] <- NULL` silently shrinks a list. Always `out[i] <- list(NULL)`
  when a slot might legitimately be `NULL`. Caught in `fit_specs_batch`
  (commit `3d76d54`).
- RStudio Console returns `FALSE` from `cli::is_dynamic_tty()` but honors
  `\r`. `dynamic_tty_supported()` opts in explicitly via `in_rstudio()`.
- `base::interactive` has a locked binding. UI gates route through
  `is_interactive_ui()` so tests can patch via `assignInNamespace`.
- `devtools::load_all()` sources `helper-isolated.R`, which used to
  clobber `texanshootR.animations` on every interactive load. Fixed by
  call-stack inspection.
