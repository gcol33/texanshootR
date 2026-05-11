# TUI manager with three rendering modes.
#
# 1. ANSI multi-zone (real terminals: Windows Terminal, MINGW64, etc.)
#    Layout (top to bottom):
#      shooter    ~6 lines  the mascot ASCII; redraws on state transition
#                           and on heartbeat tick (small gun-line change)
#      flavour    4-6 lines fixed-height ring buffer for blip stream
#      modifiers  1 line    live-available +mod tokens
#      outputs   1 line    publication-chain stages, locked / unlocked
#      bar        2 lines   progress bar + most recent loading line
#    Each zone gets a 1-line blank gutter. Row offsets are computed in
#    ui_session_open() and cached on state.
#
# 2. Dynamic single-line (RStudio Console and similar: \r honored but
#    cursor positioning isn't). All zones compress to ONE line that
#    redraws in place via \r. Composed layout, left to right:
#      [bar + pct] | face + gun | +mods... | loading-text
#    Blip stream and full ASCII art are suppressed in this mode (they
#    would force scrolling). Mascot still ticks (heartbeat) and changes
#    on emotional transitions, just as a single face+gun line.
#
# 3. Plain (CI, RMarkdown chunks, anything without \r). Sequential
#    printing, one line per update.

# ---- ANSI helpers ---------------------------------------------------

# Reliable RStudio detection. `.Platform$GUI == "RStudio"` works in
# RStudio Desktop's R Console but reports "unknown" in some
# remote/server/embedded setups. RStudio sets the RSTUDIO env var to
# "1" in *all* of its R processes, so we OR the two signals.
in_rstudio <- function() {
  identical(.Platform$GUI, "RStudio") ||
    identical(Sys.getenv("RSTUDIO"), "1")
}

# UI-activation gate. Wraps `interactive()` in a texanshootR-owned
# binding so tests can mock it via assignInNamespace -- `base::interactive`
# itself has a locked binding and can't be patched directly.
is_interactive_ui <- function() interactive()

ansi_supported <- function() {
  if (!isTRUE(opt("texanshootR.animations"))) return(FALSE)
  if (!is_interactive_ui()) return(FALSE)
  # User override: force a specific mode regardless of auto-detect.
  forced <- getOption("texanshootR.ui_mode", NA_character_)
  if (identical(forced, "ansi"))    return(TRUE)
  if (identical(forced, "dynamic")) return(FALSE)
  if (identical(forced, "plain"))   return(FALSE)
  # We need MORE than SGR colors -- cursor save/restore (\033[s/u) and
  # cursor positioning (\033[<n>A/B/H). RStudio's console honors SGR but
  # not cursor positioning, so cli::ansi_has_any() returning TRUE is not
  # enough on its own.
  if (in_rstudio()) return(FALSE)
  isTRUE(cli::ansi_has_any())
}

# Dynamic TTY: stdout respects \r so we can overwrite a single line.
# Works in RStudio console, modern Windows terminals, and most others
# where the multi-zone ANSI manager fails. We use this as a graceful
# downgrade path for the bar.
dynamic_tty_supported <- function() {
  if (!isTRUE(opt("texanshootR.animations"))) return(FALSE)
  if (!is_interactive_ui()) return(FALSE)
  forced <- getOption("texanshootR.ui_mode", NA_character_)
  if (identical(forced, "dynamic")) return(TRUE)
  if (identical(forced, "ansi"))    return(FALSE)
  if (identical(forced, "plain"))   return(FALSE)
  # RStudio's console honors \r overwrite even though cli::is_dynamic_tty()
  # returns FALSE there (it's not a real TTY). Opt in explicitly so the bar
  # redraws in place instead of printing a fresh line per percent.
  if (in_rstudio()) return(TRUE)
  tryCatch(isTRUE(cli::is_dynamic_tty()),
           error = function(e) FALSE)
}

# Diagnostic helper: prints the signals that drive mode selection.
# Call as `texanshootR:::ui_mode_diagnostic()` if the wrong mode is
# being picked.
#' @keywords internal
ui_mode_diagnostic <- function() {
  list(
    .Platform_GUI = .Platform$GUI,
    RSTUDIO_env   = Sys.getenv("RSTUDIO"),
    interactive   = interactive(),
    animations    = isTRUE(opt("texanshootR.animations")),
    forced_mode   = getOption("texanshootR.ui_mode", NA_character_),
    in_rstudio    = in_rstudio(),
    ansi          = ansi_supported(),
    dynamic       = dynamic_tty_supported(),
    cli_is_dyn    = tryCatch(cli::is_dynamic_tty(),
                              error = function(e) conditionMessage(e)),
    cli_ansi_has  = tryCatch(cli::ansi_has_any(),
                              error = function(e) conditionMessage(e))
  )
}

# In dynamic (non-rich-ANSI) mode, blanket-clear the current line with
# \r + spaces + \r so any leftover bar text is wiped before we print a
# scrolling message. RStudio's console honors \r but not \033[2K, so we
# can't rely on the line-clear escape.
dyn_clear_line <- function() {
  cat("\r", strrep(" ", getOption("width", 80L)), "\r", sep = "")
}

# Compose the single-line status from the per-zone buffers. Three slots
# only -- shooter | message | bar -- because RStudio's console isn't wide
# enough for more, and modifier injection is non-interactive in this mode
# (pre-rolled at run start, see shoot.R) so there is no live picker to
# advertise. Order: mascot anchors the left edge at a fixed pad width,
# the message is the variable middle, the bar sits flush right with a
# stable width. Empty slots are dropped so the line stays compact early
# in a run before every zone has been populated.
dyn_compose <- function(state) {
  parts <- character()
  if (nzchar(state$dyn_mascot))  parts <- c(parts, state$dyn_mascot)
  if (nzchar(state$dyn_loading)) parts <- c(parts, state$dyn_loading)
  if (nzchar(state$dyn_bar))     parts <- c(parts, state$dyn_bar)
  paste(parts, collapse = " | ")
}

# Display widths for the dynamic single-line slots. Each slot pads to
# its target so the column separators line up frame-over-frame and the
# bar never jitters left/right.
#
# Full line at the 120-col target terminal:
#
#   |- mascot 18 -| | |- loading <=70 -| | |-- bar 25 --|
#                 18 + 3 + 70 + 3 + 25 = 119 cells
#
# Mascot's max content is face(<=7) + space + gun(10) = 18 cells, so
# DYN_MASCOT_WIDTH = 18 fits the widest face with no trailing slack.
# Bar is exactly 20 fill cells + space + "100%" = 25.
#
# DYN_LOADING_BUDGET is the hard cap on bundled message text length.
# validate_registry() lints every "loading" / "state_transition" entry
# in inst/messages/*.yaml against this cap so they fit cleanly at the
# 120-col target. Narrower terminals still ellipsise -- that is the
# graceful-degradation case, not the design point.
DYN_MASCOT_WIDTH      <- 18L
DYN_BAR_WIDTH         <- 25L   # 20 cells of blocks + " " + "100%"
DYN_LOADING_BUDGET    <- 70L   # cap on bundled message text length (chars)
DYN_LOADING_WIDTH_MIN <- 20L

# Compute the loading slot's target cell width from the current terminal
# width. Read live (not at session open) so resizing the host terminal
# between renders takes effect on the next ui_loading() call. The
# reserved budget covers mascot + " | " + " | " + bar. Capped at
# DYN_LOADING_BUDGET so we don't pad the slot with excess spaces on
# very wide terminals.
dyn_loading_width <- function() {
  w <- max(DYN_LOADING_WIDTH_MIN,
           as.integer(getOption("width", 80L)) - 1L)
  reserved <- DYN_MASCOT_WIDTH + 3L + 3L + DYN_BAR_WIDTH
  min(DYN_LOADING_BUDGET,
      max(DYN_LOADING_WIDTH_MIN, w - reserved))
}

# Right-pad / truncate to a target *display* width. For mascot glyphs
# we pass in the pre-computed cell width because nchar(type="width")
# misreports box-drawing and CJK glyphs on Windows -- see the
# FACE_CELL_WIDTH / GUN_FRAME_CELL_WIDTH tables in mascot.R. For plain
# ASCII text (loading messages) we fall back to nchar(type="chars"),
# which matches cells exactly when there's no Unicode.
pad_to_width <- function(s, w, cell_width = NULL) {
  cur <- if (is.null(cell_width)) {
    tryCatch(nchar(s, type = "width"),
             error = function(e) nchar(s))
  } else {
    as.integer(cell_width)
  }
  if (cur > w) {
    if (w >= 3L) return(paste0(substr(s, 1L, w - 3L), "..."))
    return(substr(s, 1L, w))
  }
  if (cur == w) return(s)
  paste0(s, strrep(" ", w - cur))
}

# Emit the composed line via \r-overwrite. Pads to the previous length
# so trailing characters from a longer earlier render get wiped. We
# can't rely on \033[2K to clear-to-EOL because RStudio's console
# doesn't honor it, hence the explicit padding.
dyn_render <- function(state) {
  line <- dyn_compose(state)
  if (identical(line, state$dyn_last_line)) return(invisible())
  prev_len <- nchar(state$dyn_last_line %||% "")
  w <- max(20L, getOption("width", 80L) - 1L)
  if (nchar(line) > w) {
    line <- paste0(substr(line, 1L, w - 3L), "...")
  }
  state$dyn_last_line <- line
  pad <- max(0L, prev_len - nchar(line))
  cat("\r", line, strrep(" ", pad), sep = "")
  utils::flush.console()
}

ansi_save    <- function() "\033[s"
ansi_restore <- function() "\033[u"
ansi_move    <- function(row, col = 1L) sprintf("\033[%d;%dH", row, col)
ansi_up      <- function(n) if (n > 0) sprintf("\033[%dA", n) else ""
ansi_down    <- function(n) if (n > 0) sprintf("\033[%dB", n) else ""
ansi_clear_line <- function() "\033[2K"
ansi_dim     <- function(s) paste0("\033[2m", s, "\033[22m")
ansi_cyan    <- function(s) paste0("\033[36m", s, "\033[39m")

# ---- Theme palette --------------------------------------------------

style_header  <- function(s) if (ansi_supported()) ansi_cyan(s) else s
style_blip    <- function(s) if (ansi_supported()) ansi_dim(s)  else s
style_status  <- function(s) s
style_event   <- function(s) {
  if (ansi_supported()) paste0("\033[31mEVENT:\033[39m ", s)
  else paste0("EVENT: ", s)
}

# ---- Session manager ------------------------------------------------

# Reserve N blank lines below the cursor and remember the absolute row
# where the block starts. We use ESC[s/ESC[u for save/restore - if the
# host terminal moves the cursor in unexpected ways, we still draw to
# the same relative offsets.

ui_session_open <- function(blip_lines = 4L,
                            ansi = ansi_supported(),
                            dynamic = dynamic_tty_supported()) {
  shooter_h  <- length(read_body_template())
  blip_h     <- blip_lines
  modifier_h <- 1L
  output_h   <- 1L
  bar_h      <- 2L

  # Each zone is followed by a 1-line gutter, except the bar's loading
  # line which sits flush against the progress bar.
  row_blip     <- shooter_h + 1L
  row_modifier <- row_blip + blip_h + 1L
  row_output   <- row_modifier + modifier_h + 1L
  row_bar      <- row_output + output_h + 1L
  row_loading  <- row_bar + 1L
  total_h      <- row_loading + 1L

  # Environment, not list. R lists are copy-on-modify, so cache fields
  # set inside ui_set_*() would be lost when the caller (e.g.
  # maybe_animate()) discards the return value. An env mutates in place.
  state <- new.env(parent = emptyenv())
  state$ansi         <- ansi
  state$dynamic      <- !ansi && dynamic   # only use \r path when rich ANSI isn't available
  state$shooter_h    <- shooter_h
  state$blip_h       <- blip_h
  state$modifier_h   <- modifier_h
  state$output_h     <- output_h
  state$bar_h        <- bar_h
  state$row_blip     <- row_blip
  state$row_modifier <- row_modifier
  state$row_output   <- row_output
  state$row_bar      <- row_bar
  state$row_loading  <- row_loading
  state$total_h      <- total_h
  state$blips        <- character(blip_h)
  state$last_state   <- NA_character_
  state$last_tick    <- -1L
  state$last_loading <- ""
  state$last_modifiers_render <- NA_character_
  state$last_outputs_render   <- NA_character_
  state$last_progress_render  <- NA_character_
  # Dynamic single-line composed-status buffers. Each ui_set_* writer
  # updates its own slot, then dyn_render() composes them into a single
  # \r-overwritten status line. Unused in ANSI or plain mode.
  state$dyn_bar       <- ""
  state$dyn_mascot    <- ""
  state$dyn_loading   <- ""
  state$dyn_last_line <- ""
  if (ansi) {
    # Reserve space by printing newlines, then move back up.
    cat(strrep("\n", total_h))
    cat(ansi_up(total_h))
    cat(ansi_save())
  }
  state
}

ui_session_close <- function(state) {
  if (state$ansi) {
    cat(ansi_restore())
    cat(ansi_down(state$total_h))
    cat("\n")
  } else if (isTRUE(state$dynamic)) {
    # Wipe the carriage-return-overwritten line so the banner that
    # follows can claim the row instead of stacking under a leftover
    # animation frame. The animation served its purpose during the run
    # (mascot walked the emotional arc, bar filled to 100%); after the
    # run, the print.tx_run banner is the canonical end-state display,
    # and printing both produces a "duplicated shooter" effect with
    # two mascot faces stacked on top of each other.
    dyn_clear_line()
    utils::flush.console()
  } else {
    cat("\n")
  }
  invisible()
}

# ---- Zone writers ---------------------------------------------------

ui_set_mascot <- function(state, mstate, tick, cosmetics = NULL,
                          force = FALSE) {
  if (!force && identical(state$last_state, mstate) &&
      identical(state$last_tick, tick)) {
    return(state)
  }
  lines <- mascot_render(mstate, tick = tick, cosmetics = cosmetics)
  # Pad / truncate to shooter_h.
  if (length(lines) < state$shooter_h) {
    lines <- c(lines, rep("", state$shooter_h - length(lines)))
  } else if (length(lines) > state$shooter_h) {
    lines <- lines[seq_len(state$shooter_h)]
  }
  transition <- is.na(state$last_state) || !identical(state$last_state, mstate)
  if (state$ansi) {
    cat(ansi_restore())
    for (i in seq_along(lines)) {
      cat(ansi_clear_line(), lines[i], "\n", sep = "")
    }
  } else if (isTRUE(state$dynamic)) {
    # Single-line mode: render face + 2-frame dot-blink gun. Uses
    # heartbeat_frame_dyn (NOT heartbeat_frame) because the full
    # 8-frame cycle changes the wide-char composition tick-over-tick,
    # which jitters the mascot slot in RStudio Console (which renders
    # `一` at a different cell width than the standard claims). The
    # _dyn variant keeps `︻デ═一` constant and only toggles the
    # trailing `·` ↔ space, so the slot stays anchored at the same
    # column on every tick. Heartbeat ticks AND state transitions
    # both update the slot; dyn_render dedups so identical frames
    # cost nothing.
    face <- read_face(mstate)
    gun  <- heartbeat_frame_dyn(tick)
    combined  <- paste(face, gun)
    combined_w <- face_cell_width(mstate) + 1L + gun_frame_cell_width(tick)
    state$dyn_mascot <- pad_to_width(combined, DYN_MASCOT_WIDTH,
                                      cell_width = combined_w)
    dyn_render(state)
  } else if (transition) {
    # Plain mode: print only on state transition, not on every tick.
    for (l in lines) cat(l, "\n", sep = "")
  }
  state$last_state <- mstate
  state$last_tick  <- tick
  state
}

ui_blip <- function(state, text) {
  state$blips <- c(state$blips[-1L], text)
  if (state$ansi) {
    cat(ansi_restore())
    cat(ansi_down(state$row_blip))
    for (i in seq_len(state$blip_h)) {
      cat(ansi_clear_line(), style_blip(state$blips[i]), "\n", sep = "")
    }
  } else if (isTRUE(state$dynamic)) {
    # Single-line mode: blip ring buffer still updates (state$blips) so
    # message dedup / recent-buffer logic stays intact, but we don't
    # display the blip stream — it would force scrolling.
  } else {
    cat(style_blip(text), "\n", sep = "")
  }
  state
}

ui_progress <- function(state, pct, label = "") {
  pct <- max(0, min(1, pct))
  # 20 cells in both modes. In ANSI multi-zone, the bar gets its own
  # row. In dynamic single-line, the bar shares its row with mascot,
  # modifiers, and loading text, so 20 cells leaves room for the rest.
  width <- 20L
  filled <- as.integer(round(pct * width))
  # Unicode block + light-shade glyphs (U+2588, U+2591). Built via
  # intToUtf8() so this source file stays ASCII-only.
  bar_full  <- intToUtf8(9608L)
  bar_empty <- intToUtf8(9617L)
  bar <- paste0(strrep(bar_full, filled),
                strrep(bar_empty, width - filled))
  pct_lbl <- sprintf("%3d%%", as.integer(round(pct * 100)))
  line <- if (nzchar(label)) paste(bar, pct_lbl, label) else paste(bar, pct_lbl)
  if (identical(line, state$last_progress_render)) return(state)
  state$last_progress_render <- line
  if (state$ansi) {
    cat(ansi_restore())
    cat(ansi_down(state$row_bar))
    cat(ansi_clear_line(), line, "\n", sep = "")
    cat(ansi_clear_line(), "> ", style_status(state$last_loading),
        "\n", sep = "")
  } else if (isTRUE(state$dynamic)) {
    # Update just the bar slot and let dyn_render compose with the
    # other zones. Drop the `label` arg in this mode — the loading
    # text is already its own slot via ui_loading().
    state$dyn_bar <- paste(bar, pct_lbl)
    dyn_render(state)
  } else {
    cat(line, "\n", sep = "")
  }
  state
}

ui_loading <- function(state, text) {
  state$last_loading <- text
  if (state$ansi) {
    cat(ansi_restore())
    cat(ansi_down(state$row_loading))
    cat(ansi_clear_line(), "> ", style_status(text), "\n", sep = "")
  } else if (isTRUE(state$dynamic)) {
    # Pad / truncate the message to a fixed cell width so the bar slot
    # to its right stays anchored frame-over-frame. Long messages are
    # ellipsised; short ones get trailing spaces.
    state$dyn_loading <- pad_to_width(text, dyn_loading_width())
    dyn_render(state)
  } else {
    cat("> ", text, "\n", sep = "")
  }
  state
}

# Live-available modifier tokens. Renders once per shoot() batch (so the
# row tracks state$consumed_modifiers as the player burns through them).
# Cached on state$last_modifiers_render to skip redundant repaints when
# the row text hasn't changed since the last call.
ui_set_modifiers <- function(state, available_tokens) {
  line <- if (length(available_tokens))
            paste0("MODIFIERS  ",
                   format_modifier_row(available_tokens))
          else
            "MODIFIERS  (all consumed)"
  if (identical(line, state$last_modifiers_render)) return(state)
  state$last_modifiers_render <- line
  if (state$ansi) {
    cat(ansi_restore())
    cat(ansi_down(state$row_modifier))
    cat(ansi_clear_line(), style_header(line), "\n", sep = "")
  } else if (isTRUE(state$dynamic)) {
    # Dynamic single-line mode skips the modifiers row entirely. The
    # row would compete with the mascot, bar, and loading text for one
    # narrow console line, and modifier injection is non-interactive
    # here (pre-rolled at run start), so there is no live picker to
    # advertise. The cache update above is still useful for ANSI mode.
  } else {
    cat(line, "\n", sep = "")
  }
  state
}

# Publication-chain stages with locked / unlocked styling at the
# player's career tier. `unlocked_length` is the prefix length the
# player has earned (1..6). `current_due` (NA when not in chain phase)
# is the stage in [brackets] -- not used in search phase yet, reserved
# for Slice 5 when the chain plays out inside the same TUI session.
ui_set_outputs <- function(state, stages, unlocked_length,
                            current_due = NA_character_) {
  parts <- vapply(seq_along(stages), function(i) {
    s <- stages[i]
    if (!is.na(current_due) && identical(s, current_due)) {
      sprintf("[%s]", s)
    } else if (i <= unlocked_length) {
      s
    } else {
      ansi_or_plain_dim(state, s)
    }
  }, character(1))
  line <- paste0("OUTPUTS    ", paste(parts, collapse = "  "))
  if (identical(line, state$last_outputs_render)) return(state)
  state$last_outputs_render <- line
  if (state$ansi) {
    cat(ansi_restore())
    cat(ansi_down(state$row_output))
    cat(ansi_clear_line(), line, "\n", sep = "")
  } else if (isTRUE(state$dynamic)) {
    # Outputs row hidden in single-line mode — chain progression is a
    # between-runs concept, not an in-run heartbeat. The unlocked count
    # is still tracked on state for finalize_run().
  } else {
    cat(line, "\n", sep = "")
  }
  state
}

# Helper: dim if ANSI is active on this session, else passthrough.
# Kept here so the OUTPUTS renderer doesn't have to branch inline.
ansi_or_plain_dim <- function(state, s) {
  if (isTRUE(state$ansi)) ansi_dim(s) else s
}

ui_event <- function(state, event_text, consequence_text) {
  if (state$ansi) {
    cat(ansi_restore())
    cat(ansi_down(state$row_blip))
    cat(ansi_clear_line(), style_event(event_text), "\n", sep = "")
    cat(ansi_clear_line(), consequence_text, "\n", sep = "")
    Sys.sleep(0.6)
    cat(ansi_restore())
    cat(ansi_down(state$row_blip))
    for (i in seq_len(state$blip_h)) {
      cat(ansi_clear_line(), style_blip(state$blips[i]), "\n", sep = "")
    }
  } else if (isTRUE(state$dynamic)) {
    # Flash the event briefly through the loading slot. The composed
    # status line stays put; only the loading text changes.
    prev <- state$dyn_loading
    state$dyn_loading <- paste0("EVENT: ", event_text)
    dyn_render(state)
    Sys.sleep(0.6)
    state$dyn_loading <- consequence_text
    dyn_render(state)
    # Don't restore `prev` — let the next ui_loading() call overwrite.
  } else {
    cat(style_event(event_text), "\n", consequence_text, "\n", sep = "")
  }
  state
}

# ---- Plain (deadpan) message printing ------------------------------

# For one-off sober lines outside a UI session (e.g. promotion
# announcements, output status prints).
say <- function(text) {
  if (isTRUE(opt("texanshootR.quiet"))) return(invisible())
  cat(text, "\n", sep = "")
  invisible()
}
