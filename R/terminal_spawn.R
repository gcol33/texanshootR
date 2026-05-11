# Spawn a shoot() run in an external terminal so the full ANSI multi-zone
# TUI can render even when the caller is in RStudio Console (which only
# supports \r-overwrite, not cursor positioning).
#
# Flow:
#   1. Serialize df + args + the calling session's texanshootR options
#      and the package path to a temp RDS.
#   2. Write a bootstrap R script that the spawned R will source: it
#      restores the options, loads the package (devtools::load_all when
#      we're in a dev tree, library() otherwise), calls shoot(), saves
#      the result to another RDS, and pauses for Enter so the user can
#      read the post-run output before the window closes.
#   3. Launch Windows Terminal (wt.exe) with R.exe --interactive running
#      the bootstrap. Block until the terminal exits.
#   4. Read the result RDS and return it to the calling session.

spawn_shoot_in_terminal <- function(df,
                                    formula = NULL,
                                    seed    = NULL,
                                    depth   = "default",
                                    ...) {
  if (.Platform$OS.type != "windows") {
    stop("`terminal = TRUE` currently only supports Windows. ",
         "On macOS/Linux, run R from a real terminal directly.",
         call. = FALSE)
  }

  args_rds   <- tempfile("tx_args_",   fileext = ".rds")
  result_rds <- tempfile("tx_result_", fileext = ".rds")
  bootstrap  <- tempfile("tx_boot_",   fileext = ".R")
  marker     <- tempfile("tx_done_",   fileext = ".flag")

  # Forward all texanshootR.* options so the spawned R sees the same
  # save_dir, consent, budget, etc. as the calling session.
  all_opts <- options()
  tx_opts <- all_opts[grepl("^texanshootR\\.", names(all_opts))]

  saveRDS(list(
    df         = df,
    formula    = formula,
    seed       = seed,
    depth      = depth,
    extra      = list(...),
    options    = tx_opts,
    load_info  = detect_pkg_load(),
    result_rds = result_rds,
    marker     = marker
  ), args_rds)

  writeLines(c(
    "# Auto-generated bootstrap for texanshootR terminal spawn.",
    sprintf('args <- readRDS("%s")', forward_slashes(args_rds)),
    "do.call(options, args$options)",
    "li <- args$load_info",
    'if (identical(li$mode, "load_all")) {',
    '  suppressMessages(devtools::load_all(li$path, quiet = TRUE))',
    "} else {",
    '  suppressMessages(library(texanshootR))',
    "}",
    "result <- tryCatch(",
    "  do.call(texanshootR::shoot,",
    "          c(list(df = args$df, formula = args$formula,",
    "                 seed = args$seed, depth = args$depth),",
    "            args$extra)),",
    "  error = function(e) { message('shoot() error: ', conditionMessage(e)); NULL }",
    ")",
    "if (!is.null(result)) saveRDS(result, args$result_rds)",
    'file.create(args$marker)',
    'cat("\\n[shoot() finished. Press Enter to close.]\\n")',
    'invisible(tryCatch(readLines(file("stdin"), n = 1L), error = function(e) NULL))'
  ), bootstrap)

  r_exe <- file.path(R.home("bin"), "R.exe")
  wt    <- Sys.which("wt.exe")

  bootstrap_cmd <- sprintf("source('%s')", forward_slashes(bootstrap))

  if (nzchar(wt)) {
    message("Opening shoot() in Windows Terminal...")
    system2("wt.exe",
            c("-d", shQuote(getwd()),
              shQuote(r_exe),
              "--interactive", "--no-save",
              "-e", shQuote(bootstrap_cmd)),
            wait = TRUE)
  } else {
    message("wt.exe not found on PATH; opening shoot() in a new ",
            "console window via cmd.exe.")
    system2("cmd.exe",
            c("/c", "start", "/wait", "cmd", "/k",
              shQuote(r_exe), "--interactive", "--no-save",
              "-e", shQuote(bootstrap_cmd)),
            wait = TRUE)
  }

  # `wt` returns as soon as it hands off to the new window — wait=TRUE
  # doesn't block on the spawned R process. Poll the marker file
  # instead. The bootstrap touches it after readLines() unblocks, i.e.
  # after the user presses Enter.
  if (!file.exists(marker)) {
    while (!file.exists(marker)) Sys.sleep(0.2)
  }

  if (file.exists(result_rds)) {
    invisible(readRDS(result_rds))
  } else {
    message("Terminal session exited without producing a tx_run.")
    invisible(NULL)
  }
}

# Detect whether texanshootR is currently loaded via devtools::load_all
# (dev tree) or via library() (installed). The bootstrap uses this to
# pick the right load path in the spawned R.
detect_pkg_load <- function() {
  ns <- tryCatch(getNamespace("texanshootR"), error = function(e) NULL)
  if (is.null(ns)) return(list(mode = "library", path = NULL))
  path <- tryCatch(getNamespaceInfo(ns, "path"), error = function(e) NULL)
  if (is.null(path) || !dir.exists(path)) {
    return(list(mode = "library", path = NULL))
  }
  is_dev <- file.exists(file.path(path, "DESCRIPTION")) &&
            dir.exists(file.path(path, "R")) &&
            !dir.exists(file.path(path, "Meta"))
  list(mode = if (is_dev) "load_all" else "library",
       path = if (is_dev) path else NULL)
}

# Path normalization for embedding inside R-source strings: backslashes
# would be parsed as escape sequences when the bootstrap is read back.
forward_slashes <- function(p) gsub("\\\\", "/", p, fixed = FALSE)
