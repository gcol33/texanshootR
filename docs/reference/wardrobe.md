# Inspect or modify the mascot wardrobe

Without arguments, prints the equipped + unlocked cosmetics. With `slot`
and `id`, equips the given cosmetic in the given slot.

## Usage

``` r
wardrobe(slot = NULL, id = NULL)
```

## Arguments

- slot:

  Optional slot name: hat, poncho, badge, lanyard, cloak.

- id:

  Optional cosmetic id.

## Value

The wardrobe state (invisible when modifying, visible when listing).
