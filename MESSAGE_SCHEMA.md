# Message Schema

The texanshootR message engine is designed to scale to **5,000–10,000
entries** without a code change. New messages are added by dropping
YAML into `inst/messages/<category>.yaml` (or, for life-events,
`inst/events/<category>.yaml`). The package validates every entry
against this schema at load time via `validate_messages()`.

This document is the canonical contract for contributors.

---

## File layout

```
inst/messages/
├── blip.yaml                 # 1-3 word blip-stream filler
├── loading_general.yaml      # full sentence loading messages
├── derived_metrics.yaml      # desperation-escalation phase only
├── promotion.yaml            # promotion committee + advancement
├── reviewer.yaml             # Reviewer 2 encounters
├── state_transition.yaml     # one-shot mascot-state transitions
└── banner.yaml               # startup banner pool

inst/events/
└── life.yaml                 # random life-event encounters
```

One file per category keeps PR diffs readable. Files inside
`inst/messages/` share the schema below; `inst/events/` files use the
**Event schema** (separate, also below).

---

## Message schema (inst/messages/)

```yaml
- id: <unique_snake_case_id>          # required
  text: "string"                       # required, dead-serious
  tags: [tag1, tag2, ...]              # required, ≥ 1 fallacy tag
  rarity: common | uncommon | rare | legendary   # required
  trigger_phase: <vocab_phases entry>  # required
  career_min: ~ | "Junior Researcher" | "Postdoc" | "Senior Scientist" | "PI"
  model_family_affinity: [glm, mixed, gam, ...]  # optional
  mascot_state_affinity: [composed, uncertain, anxious, desperate, resolved]
  combo_chain:
    start: <chain_id> | ~              # this message *opens* a chain
    next:  <chain_id> | ~              # this message *follows* a chain
  requires: [pkg1, pkg2]               # optional; suppressed if any pkg missing
```

### Required fields

* **id** — unique across the entire registry. Convention:
  `<phase>_<topic>_<NNN>` or a meaningful slug. Validator rejects
  duplicates.
* **text** — the line shown to the user. Must read as professional,
  dead-serious output. No "lol", no emoji, no scare quotes that wink
  at the joke.
* **tags** — at least one **fallacy tag** from the first 23 entries of
  `vocab_tags`. Thematic tags (ecology, ml, ...) may be added in
  addition. Fallacy tags are how the run engine connects messages to
  the methodological sin being modelled.
* **rarity** — controls the base sampling weight. The relative scale
  is `common = 1000`, `uncommon = 100`, `rare = 10`, `legendary = 1`.
* **trigger_phase** — which `select_message()` phase will draw this
  entry. See `vocab_phases`. A message only ever fires in its phase.

### Optional fields

* **career_min** — minimum career level required to draw this
  message. `~` (YAML null) means no minimum. Useful for unlocking
  more sophisticated language at higher career tiers.
* **model_family_affinity** — list of model families this message
  pairs with (`glm`, `lm`, `mixed`, `gam`, `bayesian`, ...). When the
  caller provides a context family, messages with non-empty affinities
  must intersect it. Empty / missing means universal.
* **mascot_state_affinity** — list of mascot states this message is
  appropriate for. Used to gate desperate-feeling messages
  (`mascot_state_affinity: [anxious, desperate]`) so they don't appear
  in calm phases of the run.
* **combo_chain** — two fields:
  * `start: <id>` declares this message *opens* a chain named `<id>`.
    When drawn, the engine remembers `<id>` as the `combo_state`.
  * `next: <id>` declares this message *follows* a chain named
    `<id>`. While `combo_state` matches, follow-ups receive a 10×
    weight boost and are exempt from recency suppression.
  Chains build sequences like `correlating → causating` or
  `exploring → discovering → interpreting`.
* **requires** — list of R packages that must be available. If any is
  missing the message is dropped from the candidate pool.

---

## Event schema (inst/events/)

Life events are two-part roguelike encounters that fire ~1/6 of runs.
They have their own schema:

```yaml
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

Effect keys must come from `vocab_effects`. Numeric magnitudes are
interpreted as multiplicative for `throughput` / `search_budget` and
additive otherwise. Validator caps each effect at `[-0.5, +0.5]` so a
single event cannot dominate a run.

---

## Validation

```r
texanshootR::validate_messages()
```

Run this in tests / CI on any branch that touches the registry. The
validator enforces:

1. Required fields present.
2. Vocabulary membership: `trigger_phase ∈ vocab_phases`,
   `rarity ∈ {common, uncommon, rare, legendary}`,
   `career_min ∈ vocab_careers ∪ {NA}`,
   `tags ⊆ vocab_tags`,
   `mascot_state_affinity ⊆ vocab_mascot_states`.
3. Every message carries at least one fallacy tag.
4. Unique ids across the entire registry.
5. `combo_chain.next` only references known `combo_chain.start` ids.

---

## Style guide

* **Deadpan everywhere.** The text is funniest when it sounds like
  output from a real piece of statistical software. Avoid "satire",
  "lol", winks, scare quotes, and meta-commentary.
* **Professional polish over absurdity.** Reject any draft that reads
  as a meme. Reject any draft that reads as moralizing.
* **Concrete over abstract.** "Subsampling environmentally noisy
  observations..." beats "Doing some statistics..." — specificity
  reads as serious software.
* **Short sentences.** Loading lines should rarely exceed 80
  characters. Blips should be 1–3 words.
* **Tag everything.** A single fallacy tag is enough but more is
  better. Tags are how the engine builds run coherence.

---

## Adding a new category

1. Drop a new YAML file into `inst/messages/<category>.yaml`.
2. If you need a new `trigger_phase`, add it to `R/vocab.R`'s
   `vocab_phases` first.
3. Run `texanshootR::validate_messages()` locally — it returns the
   parsed registry and surfaces any errors with file + entry index.
4. Send a PR. Be ready to defend any entry that risks breaking
   deadpan.
