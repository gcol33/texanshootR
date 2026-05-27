# Message Schema

Every line the TUI prints comes from a YAML entry: loading text, blips,
state transitions, promotions, reviewer encounters, and the startup
banner live under `inst/messages/`; life events live under
`inst/events/`. Messages are data, not code — adding one is a YAML edit
plus a re-run of
[`validate_messages()`](https://gillescolling.com/texanshootR/reference/validate_messages.md),
with no change to R source.

[`validate_messages()`](https://gillescolling.com/texanshootR/reference/validate_messages.md)
loads every file and checks it against the schema below. The test suite
runs it on every build, so a malformed entry fails CI. This document is
the field reference.

------------------------------------------------------------------------

## File layout

    inst/messages/
    ├── blip.yaml                 # 1-3 word blip-stream filler
    ├── loading_general.yaml      # full-sentence loading messages
    ├── derived_metrics.yaml      # desperation-escalation phase only
    ├── promotion.yaml            # promotion committee + advancement
    ├── reviewer.yaml             # Reviewer 2 encounters
    ├── state_transition.yaml     # one-shot mascot-state transitions
    └── banner.yaml               # startup banner pool

    inst/events/
    └── life.yaml                 # random life-event encounters

One file per category keeps PR diffs readable. Files under
`inst/messages/` use the message schema; files under `inst/events/` use
the event schema. Both are below.

------------------------------------------------------------------------

## Message schema (`inst/messages/`)

``` yaml
- id: <unique_snake_case_id>          # required
  text: "string"                       # required
  rarity: common | uncommon | rare | legendary   # required
  trigger_phase: <vocab_phases entry>  # required
  tags: [tag1, tag2, ...]              # see "Tags" below
  career_min: ~ | "Junior Researcher" | "Postdoc" | "Senior Scientist" | "PI"
  model_family_affinity: [glm, mixed, gam, ...]  # optional
  mascot_state_affinity: [composed, uncertain, anxious, desperate, resolved]
  combo_chain:
    start: <chain_id> | ~              # this message opens a chain
    next:  <chain_id> | ~              # this message follows a chain
  requires: [pkg1, pkg2]               # optional; entry dropped if any pkg missing
```

### Required fields

The validator rejects an entry that omits any of these four.

- **id** — unique across the entire registry. Convention:
  `<phase>_<topic>_<NNN>` or a meaningful slug.
- **text** — the line shown to the user. Written as statistical-software
  status output: a complete clause, no emoji, no chat shorthand. For the
  `loading` and `state_transition` phases it must fit the single-line
  budget (see Validation).
- **rarity** — the base sampling weight. The relative scale is
  `common = 1000`, `uncommon = 100`, `rare = 10`, `legendary = 1`.
- **trigger_phase** — which `select_message()` phase draws the entry.
  One of `vocab_phases`. A message only ever fires in its phase.

### Tags

`tags` is a list of values drawn from `vocab_tags`. It splits into two
kinds:

- **Fallacy tags** — the first 25 entries of `vocab_tags`
  (`texas_sharpshooter`, `p_hacking`, `harking`, …). These connect a
  message to the methodological sin it models.
- **Thematic tags** — the remainder (`ecology`, `ml`, `reviewer`, …),
  added for topical filtering.

Messages in a phase that *is* the methodological mechanic — `blip`,
`loading`, `derived_escalation`, `ultra_rare`, `event`,
`event_consequence` — must carry at least one fallacy tag. Ceremony
phases (`banner`, `promotion`, `reviewer`, `state_transition`, `daily`,
and the chain-transition phases) are exempt; `tags` may be omitted for
them.

### Optional fields

- **career_min** — minimum career level required to draw the message.
  `~` (YAML null) means no minimum. One of `vocab_careers` otherwise.
- **model_family_affinity** — model families the message pairs with
  (`glm`, `lm`, `mixed`, `gam`, `bayesian`, …). When the caller passes a
  context family, a message with a non-empty affinity is drawn only if
  its list intersects that family. Empty or missing means universal.
- **mascot_state_affinity** — mascot states the message is appropriate
  for, drawn from `vocab_mascot_states`. Gates state-specific lines so a
  desperate line does not surface in a composed phase.
- **combo_chain** — two sub-fields:
  - `start: <id>` marks this message as opening a chain named `<id>`.
    When drawn, the engine records `<id>` as the current `combo_state`.

  - `next: <id>` marks this message as following chain `<id>`. While
    `combo_state` matches, follow-ups get a 10x weight boost and skip
    recency suppression.

    Chains build sequences such as `correlating -> causating` or
    `exploring -> discovering -> interpreting`.
- **requires** — R packages that must be installed. If any is missing
  the entry is dropped from the candidate pool at runtime.

------------------------------------------------------------------------

## Event schema (`inst/events/`)

Life events are two-part encounters that fire in roughly one run in six.

``` yaml
- id: <unique_snake_case_id>          # required
  event_text: "string"                 # required, the EVENT: line
  consequence_text: "string"           # required, second line
  tags: [admin, ...]                   # required
  rarity: common | uncommon | rare | legendary   # required
  career_min: ~ | <career level>       # optional
  display_phase: any | early | mid | late | desperation
  effects:                             # optional; vocab_effects keys
    throughput: -0.15
    typo_probability: 0.10
```

Effect keys come from `vocab_effects`. Magnitudes are multiplicative for
`throughput` and `search_budget`, additive otherwise. Each effect is
capped to `[-0.5, +0.5]` on load so one event cannot dominate a run.

------------------------------------------------------------------------

## Validation

``` r

texanshootR::validate_messages()
```

It loads the registry and enforces, in order:

1.  **Required fields** present on every message (`id`, `text`,
    `rarity`, `trigger_phase`).
2.  **Unique ids** across the whole registry.
3.  **Vocabulary membership**: `trigger_phase` in `vocab_phases`;
    `rarity` in `{common, uncommon, rare, legendary}`; `career_min` in
    `vocab_careers` (when not null); every tag in `vocab_tags`; every
    `mascot_state_affinity` in `vocab_mascot_states`.
4.  **Fallacy tag** present on every message in a mechanic phase
    (`blip`, `loading`, `derived_escalation`, `ultra_rare`, `event`,
    `event_consequence`).
5.  **Combo-chain integrity**: every `combo_chain.next` resolves to a
    known `combo_chain.start`.
6.  **Single-line budget**: `text` in the `loading` and
    `state_transition` phases is at most `DYN_LOADING_BUDGET` (70)
    characters. These phases share one slot with the mascot and progress
    bar in the dynamic single-line TUI; longer text would be ellipsised
    at the 120-column target width.

Any failure stops with the offending file, entry, and value.

------------------------------------------------------------------------

## Adding a category

1.  Drop a new YAML file into `inst/messages/<category>.yaml`.
2.  If the messages need a new `trigger_phase`, add it to `vocab_phases`
    in `R/vocab.R` first.
3.  Run
    [`texanshootR::validate_messages()`](https://gillescolling.com/texanshootR/reference/validate_messages.md).
    It returns the parsed registry and reports the first error with its
    file and entry index.
4.  Open a PR.
