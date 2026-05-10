# Package options for texanshootR

All options are read at the time of use, so changing them takes effect
immediately for subsequent calls.

## Details

- `texanshootR.animations`:

  Logical. Enable terminal animations and ANSI cursor effects. Default:
  `TRUE`. Forced off in non-interactive sessions and when the terminal
  lacks ANSI support.

- `texanshootR.output_dir`:

  Character or `NULL`. Default directory for output files. `NULL` falls
  back to [`tempdir()`](https://rdrr.io/r/base/tempfile.html).

- `texanshootR.save_enabled`:

  Logical. Enable persistent career data. `FALSE` runs entirely in
  memory. Default: `TRUE`.

- `texanshootR.quiet`:

  Logical. Suppress status prints from output functions. Default:
  `FALSE`.

- `texanshootR.event_rate`:

  Numeric in `[0, 1]`. Per-run probability of a life-event encounter.
  Default: `1/6`.

- `texanshootR.life_events`:

  Logical. Enable life-event encounters during runs. Default: `TRUE`.
