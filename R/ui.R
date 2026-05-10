# Three-zone TUI manager.
#
# Layout (top to bottom):
#   shooter   ~6 lines  the mascot ASCII; redraws on state transition
#                       and on heartbeat tick (small gun-line change)
#   flavour   4-6 lines fixed-height ring buffer for blip stream
#   bar       2 lines   progress bar + the most recent loading line
#
# When ANSI cursor control is unavailable or animations are disabled,
# the manager falls back to plain sequential printing so the package
# still works in CI, RStudio with limited TTY, RMarkdown chunks, etc.

# ---- ANSI helpers ---------------------------------------------------

ansi_supported <- function() {
  if (!isTRUE(opt("texanshootR.animations"))) return(FALSE)
  if (!interactive()) return(FALSE)
  # cli's check is the most portable.
  isTRUE(cli::ansi_has_any())
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
                            ansi = ansi_supported()) {
  shooter_h <- length(read_body_template())
  bar_h     <- 2L
  total_h   <- shooter_h + 1L + blip_lines + 1L + bar_h
  state <- list(
    ansi      = ansi,
    shooter_h = shooter_h,
    blip_h    = blip_lines,
    bar_h     = bar_h,
    total_h   = total_h,
    blips     = character(blip_lines),
    last_state = NA_character_,
    last_tick  = -1L,
    last_loading = ""
  )
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
  if (state$ansi) {
    cat(ansi_restore())
    for (i in seq_along(lines)) {
      cat(ansi_clear_line(), lines[i], "\n", sep = "")
    }
  } else if (is.na(state$last_state) || !identical(state$last_state, mstate)) {
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
    # Move to the start of the blip zone (after shooter + 1 blank).
    cat(ansi_restore())
    cat(ansi_down(state$shooter_h + 1L))
    for (i in seq_len(state$blip_h)) {
      cat(ansi_clear_line(), style_blip(state$blips[i]), "\n", sep = "")
    }
  } else {
    cat(style_blip(text), "\n", sep = "")
  }
  state
}

ui_progress <- function(state, pct, label) {
  pct <- max(0, min(1, pct))
  width <- 20L
  filled <- as.integer(round(pct * width))
  # Unicode block + light-shade glyphs (U+2588, U+2591). Built via
  # intToUtf8() so this source file stays ASCII-only.
  bar_full  <- intToUtf8(9608L)
  bar_empty <- intToUtf8(9617L)
  bar <- paste0(strrep(bar_full, filled),
                strrep(bar_empty, width - filled))
  pct_lbl <- sprintf("%3d%%", as.integer(round(pct * 100)))
  line <- paste(bar, pct_lbl, label)
  if (state$ansi) {
    cat(ansi_restore())
    cat(ansi_down(state$shooter_h + 1L + state$blip_h + 1L))
    cat(ansi_clear_line(), line, "\n", sep = "")
    cat(ansi_clear_line(), "> ", style_status(state$last_loading),
        "\n", sep = "")
  } else {
    cat(line, "\n", sep = "")
  }
  state
}

ui_loading <- function(state, text) {
  state$last_loading <- text
  if (state$ansi) {
    cat(ansi_restore())
    cat(ansi_down(state$shooter_h + 1L + state$blip_h + 1L + 1L))
    cat(ansi_clear_line(), "> ", style_status(text), "\n", sep = "")
  } else {
    cat("> ", text, "\n", sep = "")
  }
  state
}

ui_event <- function(state, event_text, consequence_text) {
  if (state$ansi) {
    # Render as a transient overlay in the blip zone and refresh.
    cat(ansi_restore())
    cat(ansi_down(state$shooter_h + 1L))
    cat(ansi_clear_line(), style_event(event_text), "\n", sep = "")
    cat(ansi_clear_line(), consequence_text, "\n", sep = "")
    Sys.sleep(0.6)
    # Repaint the blip zone afterward.
    cat(ansi_restore())
    cat(ansi_down(state$shooter_h + 1L))
    for (i in seq_len(state$blip_h)) {
      cat(ansi_clear_line(), style_blip(state$blips[i]), "\n", sep = "")
    }
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
