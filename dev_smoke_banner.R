## Smoke test for the new sectioned print.tx_run banner.
## Runs once with a budget guaranteed to land a shippable result, then
## once with a budget that should miss to verify the non-shippable path.

options(texanshootR.animations = FALSE,
        texanshootR.quiet      = TRUE,
        texanshootR.consent    = TRUE,
        texanshootR.budget     = 2L,
        texanshootR.life_events = FALSE)

devtools::load_all(quiet = TRUE)

cat("\n--- shippable (mtcars typically hits p<=0.05 quickly) ---\n\n")
set.seed(11)
run <- shoot(mtcars)
print(run)

cat("\n\n--- non-shippable (iris is harder under a 2s budget) ---\n\n")
# A frame where the search has less low-hanging fruit. We don't
# strictly know this misses, but it's a reasonable trial.
hard <- data.frame(y = rnorm(40),
                   x1 = rnorm(40), x2 = rnorm(40), x3 = rnorm(40))
set.seed(99)
run2 <- shoot(hard)
print(run2)
