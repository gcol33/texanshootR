# Render the mascot for a given state and heartbeat tick.

Render the mascot for a given state and heartbeat tick.

## Usage

``` r
mascot_render(state, tick = 0L, cosmetics = NULL)
```

## Arguments

- state:

  One of the five mascot states.

- tick:

  Integer tick counter for the heartbeat animation.

- cosmetics:

  Optional wardrobe state list (slot -\> equipped id).

## Value

Character vector of lines.
