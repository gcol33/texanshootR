# Smoke test for the live-modifier registry in R/modifiers_live.R.
#
# Verifies:
#   1. Registry shape -- every entry carries the required fields.
#   2. Parser accepts every documented input form.
#   3. Empty / unknown input returns NA_character_.
#   4. available_modifiers() respects consumed + career gates.
#   5. apply_modifier() consumes the token, queues the bias, fires the
#      time-bonus callback exactly once.
#   6. drain_pending_bias() pops FIFO.

suppressPackageStartupMessages({
  devtools::load_all(quiet = TRUE)
})

reg <- texanshootR:::LIVE_MODIFIERS
parse  <- texanshootR:::parse_mod_command
avail  <- texanshootR:::available_modifiers
apply_ <- texanshootR:::apply_modifier
drain  <- texanshootR:::drain_pending_bias
format_row <- texanshootR:::format_modifier_row

cat("---- 1. registry shape ----\n")
required <- c("token", "aliases", "display", "time_bonus", "bias", "description")
for (token in names(reg)) {
  e <- reg[[token]]
  stopifnot(all(required %in% names(e)))
  stopifnot(identical(e$token, token))
  stopifnot(is.character(e$aliases))
  stopifnot(is.character(e$display) && nzchar(e$display))
  stopifnot(is.integer(e$time_bonus) || is.numeric(e$time_bonus))
  stopifnot(is.list(e$bias))
}
cat(sprintf("OK -- %d entries all well-formed.\n", length(reg)))

cat("\n---- 2. parser positive cases ----\n")
positive_cases <- list(
  "+glmm"       = "glmm",
  "+ glmm"      = "glmm",
  "glmm"        = "glmm",
  "+glmm()"     = "glmm",
  "+glmm( )"    = "glmm",
  "  +GLMM  "   = "glmm",
  "  glmm  "    = "glmm",
  "+derived"    = "derived_metrics",
  "+derived_metrics" = "derived_metrics",
  "+ dm"        = "derived_metrics",
  "DM"          = "derived_metrics",
  "+sub"        = "subgroup",
  "+SG"         = "subgroup",
  "+out"        = "outliers"
)
for (input in names(positive_cases)) {
  got <- parse(input)
  want <- positive_cases[[input]]
  if (!identical(got, want)) {
    stop(sprintf("parse(%s) returned %s, expected %s",
                 dQuote(input), dQuote(got %||% "NA"), dQuote(want)))
  }
}
cat(sprintf("OK -- %d inputs parsed correctly.\n", length(positive_cases)))

cat("\n---- 3. parser negative cases ----\n")
negative_cases <- c("", "  ", "+", "+ ", "+nonsense", "fakemod", "+glmm extra")
for (input in negative_cases) {
  got <- parse(input)
  if (!is.na(got)) {
    stop(sprintf("parse(%s) returned %s, expected NA",
                 dQuote(input), dQuote(got)))
  }
}
cat(sprintf("OK -- %d unknown inputs returned NA.\n", length(negative_cases)))

cat("\n---- 4. availability with career + consumed ----\n")
# Junior researcher: only lm-family + tier-agnostic mods. glmm/sem are PI-only.
junior_av <- avail(consumed = character(), career_level = "Junior Researcher")
stopifnot(!"glmm" %in% junior_av)
stopifnot(!"sem"  %in% junior_av)
stopifnot("derived_metrics" %in% junior_av)
stopifnot("subgroup"        %in% junior_av)
cat(sprintf("Junior:   %s\n", paste(junior_av, collapse = ", ")))

# PI: all available.
pi_av <- avail(consumed = character(), career_level = "PI")
stopifnot("glmm" %in% pi_av)
stopifnot("sem"  %in% pi_av)
cat(sprintf("PI:       %s\n", paste(pi_av, collapse = ", ")))

# After consuming glmm, it disappears.
pi_av_after <- avail(consumed = "glmm", career_level = "PI")
stopifnot(!"glmm" %in% pi_av_after)
stopifnot("sem" %in% pi_av_after)
cat(sprintf("PI-glmm:  %s\n", paste(pi_av_after, collapse = ", ")))

cat("\n---- 5. apply_modifier end-to-end ----\n")
state <- list()
deadline_extension <- 0L
extend <- function(secs) {
  deadline_extension <<- deadline_extension + secs
}
state <- apply_(state, "glmm", time_bonus_callback = extend)
stopifnot("glmm" %in% state$consumed_modifiers)
stopifnot(length(state$pending_bias) == 1L)
stopifnot(identical(state$pending_bias[[1L]]$family, "glmm"))
stopifnot(identical(state$last_modifier$token, "glmm"))
stopifnot(deadline_extension == 5L)
cat(sprintf("glmm applied: consumed=%s, bias=%s, +%ds\n",
            paste(state$consumed_modifiers, collapse = ","),
            state$pending_bias[[1L]]$family,
            deadline_extension))

# Second injection: subgroup.
state <- apply_(state, "subgroup", time_bonus_callback = extend)
stopifnot(length(state$pending_bias) == 2L)
stopifnot(identical(state$pending_bias[[2L]]$perturb_tag, "subgroup"))
stopifnot(deadline_extension == 7L)  # 5 + 2
cat(sprintf("subgroup applied: pending=%d, total +%ds\n",
            length(state$pending_bias), deadline_extension))

cat("\n---- 6. drain_pending_bias FIFO ----\n")
res1 <- drain(state)
stopifnot(identical(res1$bias$family, "glmm"))
res2 <- drain(res1$state)
stopifnot(identical(res2$bias$perturb_tag, "subgroup"))
res3 <- drain(res2$state)
stopifnot(is.null(res3$bias))
cat("OK -- biases drained in injection order.\n")

cat("\n---- 7. TUI row formatter ----\n")
cat(sprintf("'%s'\n", format_row(c("gam", "derived_metrics", "subgroup"))))
cat(sprintf("'%s'\n", format_row(character())))

cat("\n---- 8. tactical_pause branches (mocked readline) ----\n")
pause <- texanshootR:::tactical_pause
ui_open  <- texanshootR:::ui_session_open
# Open a real UI session so the loading lines have somewhere to land
# even in plain (non-ANSI) mode.
ui <- ui_open()

mkprompt <- function(typed) function(prompt) typed

# (a) skip — empty input.
state <- list()
res <- pause(state, end_time = 100, ui = ui, career_level = "PI",
             prompt_fn = mkprompt(""))
stopifnot(is.na(res$applied))
stopifnot(res$end_time == 100)
stopifnot(length(res$state$consumed_modifiers %||% character()) == 0L)
cat("(a) skip on empty input:  OK\n")

# (b) garbage input.
res <- pause(state, end_time = 100, ui = ui, career_level = "PI",
             prompt_fn = mkprompt("+fake"))
stopifnot(is.na(res$applied))
stopifnot(res$end_time == 100)
cat("(b) skip on unknown mod:  OK\n")

# (c) career-locked — Junior tries +glmm.
res <- pause(state, end_time = 100, ui = ui,
             career_level = "Junior Researcher",
             prompt_fn = mkprompt("+glmm"))
stopifnot(is.na(res$applied))
stopifnot(res$end_time == 100)
cat("(c) lock on under-tier:   OK\n")

# (d) consumed — pre-fill state$consumed_modifiers.
res <- pause(list(consumed_modifiers = "glmm"), end_time = 100, ui = ui,
             career_level = "PI", prompt_fn = mkprompt("+glmm"))
stopifnot(is.na(res$applied))
stopifnot(res$end_time == 100)
cat("(d) lock on consumed:     OK\n")

# (e) successful injection — PI types +glmm.
res <- pause(state, end_time = 100, ui = ui, career_level = "PI",
             prompt_fn = mkprompt("+glmm"))
stopifnot(identical(res$applied, "glmm"))
stopifnot(res$end_time == 105L)
stopifnot("glmm" %in% res$state$consumed_modifiers)
stopifnot(length(res$state$pending_bias) == 1L)
cat(sprintf("(e) apply +glmm at PI:    OK (end_time 100 -> %d)\n",
            res$end_time))

# (f) alias accepted — +dm resolves to derived_metrics.
res <- pause(state, end_time = 100, ui = ui, career_level = "Junior Researcher",
             prompt_fn = mkprompt("+dm"))
stopifnot(identical(res$applied, "derived_metrics"))
stopifnot(res$end_time == 105L)
cat(sprintf("(f) alias +dm resolves:   OK (end_time 100 -> %d)\n",
            res$end_time))

cat("\nSmoke OK.\n")
