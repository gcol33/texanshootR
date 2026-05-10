# Cinematic ANSI animation helpers, used only for the single ultra-rare
# event in v1 ("The Glimpse of Tenure"). Everything is gated behind
# `interactive()` + ANSI capability + `texanshootR.animations` so it
# never fires in tests or under R CMD check.

ultra_rare_glimpse <- function() {
  if (!ansi_supported()) return(invisible())
  frames <- list(
    c("",
      "     A SLIVER OF SOMETHING DEFINITE PASSES THROUGH THE HALLWAY.",
      ""),
    c("",
      "                       *",
      ""),
    c("",
      "                  . A GLIMPSE .",
      ""),
    c("",
      "                  TENURE-ADJACENT.",
      "")
  )
  for (f in frames) {
    for (l in f) cat(l, "\n", sep = "")
    Sys.sleep(0.7)
    cat(ansi_up(length(f)))
    for (l in f) cat(ansi_clear_line(), "\n", sep = "")
    cat(ansi_up(length(f)))
  }
  invisible()
}
