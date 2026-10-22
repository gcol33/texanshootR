# Story: texanshootR

## Claim
A satirical, reproducible specification search that keeps fitting models until one reaches `p <= 0.05`, then generates the paper, talk, reviewer response and grant around it.

## Audience
R users who know what p-hacking is; the package is a teaching and satire tool, played in the terminal.

## Relation
- to: none

## Arcs

### A1 The search
- question: what does `shoot()` do?
- answer: a budgeted, seeded search over subsets, transformations, interactions, exclusions, subgroups and model families, with the seed and trace hash stored for replay
- home: vignettes/getting-started.Rmd ("A first run")
- also: README

### A2 The publication chain
- question: what happens after a hit?
- answer: six generators redeemed in order, with XP and structured chain errors
- home: vignettes/getting-started.Rmd ("The publication chain")
- also: README

### A3 Career and unlocks
- question: why do some families not appear?
- answer: career tier from XP gates the family pool; achievements and wardrobe
- home: vignettes/getting-started.Rmd ("Career, achievements, cosmetics")
- also: README

### A4 The message pack
- question: where does the commentary come from, and how do I add a line?
- answer: YAML registry with fallacy tags, validated by `validate_messages()`
- home: MESSAGE_SCHEMA.md
- also: README

## Non-goals
Not stated in the docs.

## Documents
- DESCRIPTION: deadpan claim, search axes, outputs
- README / landing: claim, list of arcs, installation (compiled), first run, routing
- Get started (getting-started.Rmd): first run, chain, career, persistent state, resetting

## Open
- The README's former satirical Support footer (Big Lebowski quote) was replaced with the shared footer; the author may want the old one back.
