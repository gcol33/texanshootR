## Verify the new dynamic single-line layout:
##  - order: shooter | message (loading) | bar
##  - shooter slot pads to a fixed width regardless of face/gun glyphs
##  - modifiers row is suppressed in dynamic mode (auto-rolled at run start)
##
## Run with: Rscript dev_smoke_dyn.R

options(texanshootR.animations = TRUE,
        texanshootR.quiet      = FALSE,
        texanshootR.consent    = TRUE,
        texanshootR.budget     = 5L)

devtools::load_all(quiet = TRUE)

cat("\n--- dyn manager direct test ---\n")
s <- texanshootR:::ui_session_open(ansi = FALSE, dynamic = TRUE)

# Mascot at the iconic composed state.
texanshootR:::ui_set_mascot(s, "composed", tick = 0L)
cat("\n  after mascot 'composed' tick 0: '", s$dyn_last_line, "'\n", sep = "")

# Loading text comes second in the new order (shooter | message | bar).
texanshootR:::ui_loading(s, "running glm hp")
cat("  after loading text:            '", s$dyn_last_line, "'\n", sep = "")

# Bar slot lands last.
texanshootR:::ui_progress(s, 0.10)
cat("  after bar 10%:                 '", s$dyn_last_line, "'\n", sep = "")

# Modifier row is a no-op in dynamic mode -- pre-roll happens at run
# start in shoot(), not via the live picker, so the line stays compact.
all_mods <- names(texanshootR:::LIVE_MODIFIERS)
prev_line <- s$dyn_last_line
texanshootR:::ui_set_modifiers(s, all_mods)
stopifnot(identical(s$dyn_last_line, prev_line))
cat("  ui_set_modifiers no-op:        OK\n", sep = "")

# Swap face to a wider one -- bar column may shift since loading text
# occupies the middle, but the layout stays well-formed.
texanshootR:::ui_set_mascot(s, "desperate", tick = 3L)
cat("  after face swap to desperate:  '", s$dyn_last_line, "'\n", sep = "")

texanshootR:::ui_session_close(s)

cat("\n--- pre_roll_modifiers ---\n")
set.seed(1)
roll <- texanshootR:::pre_roll_modifiers("Junior Researcher")
cat("rolled:\n")
for (k in names(roll)) cat("  ", k, " -> ", roll[[k]], "\n", sep = "")

cat("\n--- demo shoot ---\n")
run <- shoot(mtcars, depth = "demo")
cat("demo OK; spec_count =", run$spec_count, "\n")
