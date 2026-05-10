options(texanshootR.save_dir   = tempfile("tx-smoke-"),
        texanshootR.consent    = TRUE,
        texanshootR.animations = FALSE,
        texanshootR.quiet      = FALSE,
        texanshootR.life_events = FALSE)
suppressWarnings(devtools::load_all(quiet = TRUE))
set.seed(7)
run <- shoot(mtcars, theatrical = FALSE, budget = 3, escalate = FALSE)
cat("\nRUN OK, spec_count =", run$spec_count, "\n\n")
print(run)
cat("\n--- career ---\n")
print(career())
cat("\n--- summary(run) ---\n")
print(summary(run))
