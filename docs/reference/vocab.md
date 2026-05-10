# Controlled vocabularies for the message and event registries

These exported character vectors define the valid values for the `tags`,
`career_min`, `trigger_phase`, `mascot_state_affinity`, and event
`effects` fields used in `inst/messages/*.yaml` and
`inst/events/*.yaml`.
[`validate_messages()`](https://gillescolling.com/texanshootR/reference/validate_messages.md)
enforces them.

## Usage

``` r
vocab_tags

vocab_phases

vocab_careers

vocab_mascot_states

vocab_effects

career_levels()
```

## Format

An object of class `character` of length 37.

An object of class `character` of length 12.

An object of class `character` of length 4.

An object of class `character` of length 5.

An object of class `character` of length 9.

## Functions

- `vocab_tags`: Valid `tags` values. The lower-case fallacy names from
  the design contract plus thematic content tags.

- `vocab_phases`: Valid `trigger_phase` values.

- `vocab_careers`: Valid `career_min` values.

- `vocab_mascot_states`: Valid `mascot_state_affinity` values.

- `vocab_effects`: Valid event-effect keys for `inst/events/*.yaml`.

- `career_levels()`: Career level ordering (low to high).
