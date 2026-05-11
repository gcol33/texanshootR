suppressPackageStartupMessages(devtools::load_all(quiet = TRUE))

td <- file.path(tempdir(), paste0("tx-test-", as.integer(Sys.time())))
dir.create(td, showWarnings = FALSE, recursive = TRUE)
options(
  texanshootR.save_dir     = td,
  texanshootR.save_enabled = TRUE,
  texanshootR.consent      = TRUE,
  texanshootR.animations   = FALSE,
  texanshootR.quiet        = TRUE,
  texanshootR.life_events  = FALSE
)

# Mimic test-abstract.R's stateful setup: bump unlock to length 2, open a chain.
meta <- texanshootR:::read_meta() %||%
        texanshootR:::default_meta(getOption("texanshootR.save_dir"))
meta$progression$length_unlocked <- 2L
meta$progression$xp <- texanshootR:::CHAIN_XP_THRESHOLDS[2L]
meta <- texanshootR:::open_chain(meta, "STATE_LEAK_RUN")
texanshootR:::write_meta(meta)

# Instrument fit_specs_batch via unlockBinding.
ns <- asNamespace("texanshootR")
orig_fsb <- ns$fit_specs_batch
unlockBinding("fit_specs_batch", ns)
assign("fit_specs_batch", function(df, outcome, specs) {
  fams <- vapply(specs, function(s) s$family$fitter %||% "lm", character(1))
  cat(sprintf("[FSB] in=%d  fams=[%s]\n",
              length(specs), paste(fams, collapse = ",")))
  res <- orig_fsb(df, outcome, specs)
  cat(sprintf("[FSB] out=%d  null_slots=%d\n",
              length(res), sum(vapply(res, is.null, logical(1)))))
  if (length(res) < length(specs)) {
    cat("[FSB] !! LENGTH MISMATCH !!\n")
  }
  res
}, envir = ns)
lockBinding("fit_specs_batch", ns)

withr::local_options(texanshootR.budget = 2)
withr::local_seed(42)
run <- tryCatch(shoot(mtcars),
                error = function(e) {
                  cat("\nERROR:", conditionMessage(e), "\n")
                  print(rlang::trace_back())
                  NULL
                })
cat("\nDone. spec_count =", run$spec_count %||% NA, "\n")
