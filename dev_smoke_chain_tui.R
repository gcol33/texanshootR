# Smoke test for the chain TUI hookup. Verifies:
#   1. The message registry validates with the new chain phases.
#   2. select_message() can find an entry for each new phase.
#   3. The chain announcement helpers produce expected output.

suppressPackageStartupMessages({
  devtools::load_all(quiet = TRUE)
})

cat("---- 1. validate_messages() ----\n")
reg <- validate_messages()
cat(sprintf("Total entries: %d\n", nrow(reg)))
cat("Phases present:\n")
print(table(reg$trigger_phase))

cat("\n---- 2. select_message() probes ----\n")
set.seed(42)
for (ph in c("chain_opened", "stage_advanced",
             "chain_completed", "chain_broken")) {
  state <- switch(ph,
    chain_opened     = "polishing",
    stage_advanced   = "submitting",
    chain_completed  = "granted",
    chain_broken     = "rejected")
  d <- texanshootR:::select_message(phase = ph, mascot_state = state)
  cat(sprintf("%-18s -> %s\n", ph, if (is.null(d)) "<none>" else d$text))
}

cat("\n---- 3. Chain helpers (live output) ----\n")
fake_meta <- list(
  active_chain = list(
    run_id = "SMOKE",
    stage_idx = 2L,
    deadline = as.numeric(Sys.time()) + 25,
    completed_stages = "abstract",
    opened_at = as.numeric(Sys.time())
  ),
  progression = list(xp = 6L, length_unlocked = 2L,
                      chains_completed = 0L, chains_broken = 0L)
)
attr(fake_meta, "xp_awarded") <- 1L
options(texanshootR.quiet = FALSE)

cat("\nsay_chain_opened():\n")
texanshootR:::say_chain_opened(fake_meta)

cat("\nsay_stage_landed(meta, 'abstract'):\n")
texanshootR:::say_stage_landed(fake_meta, "abstract")

cat("\nsay_chain_broken('window_expired', c('abstract')):\n")
texanshootR:::say_chain_broken("window_expired", c("abstract"))

cat("\n---- 4. Mascot faces for new states ----\n")
for (s in c("polishing", "submitting", "granted", "rejected")) {
  cat(sprintf("%-12s %s\n", s, texanshootR:::read_face(s)))
}

cat("\nSmoke OK.\n")
