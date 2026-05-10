options(texanshootR.save_dir   = tempfile("tx-smoke-"),
        texanshootR.consent    = TRUE,
        texanshootR.animations = FALSE,
        texanshootR.quiet      = FALSE,
        texanshootR.life_events = FALSE,
        texanshootR.budget     = 20)
suppressWarnings(devtools::load_all(quiet = TRUE))
set.seed(7)
run <- shoot(mtcars)
cat("\nRUN OK, spec_count =", run$spec_count, "\n\n")
print(run)
cat("\n--- career ---\n")
print(career())
cat("\n--- summary(run) ---\n")
print(summary(run))
