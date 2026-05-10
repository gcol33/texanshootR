# Reset persisted texanshootR state

These functions remove pieces of the persistent save written under
[`tools::R_user_dir()`](https://rdrr.io/r/tools/userdir.html). They
prompt for confirmation in interactive sessions; pass `force = TRUE` to
suppress the prompt. Non-interactive sessions require `force = TRUE`.

## Usage

``` r
reset_career(force = FALSE)

reset_achievements(force = FALSE)

reset_wardrobe(force = FALSE)

reset_all(force = FALSE)
```

## Arguments

- force:

  Logical. Skip the confirmation prompt.

## Value

`TRUE` invisibly on success, `FALSE` if cancelled.
