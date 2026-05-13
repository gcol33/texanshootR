# The shooter -- the package's protagonist.
#
# Every `shoot()` call is one of his runs. His emotional state is the
# spine the rest of the system hangs off: it drives the TUI face, the
# achievement triggers, the print() banner on `tx_run`, and the run
# arc summary recorded on disk. Treat this file as the protagonist's
# state machine, not a rendering helper.
#
# Two layers of animation render that state:
#   1. EMOTIONAL STATE (slow): face changes on `mascot_state` transitions
#      driven by run progress + best p-value. Seven states: composed,
#      uncertain, worried, anxious, panicked, desperate, resolved.
#   2. HEARTBEAT (fast): tiny 8-frame cycle on the gun line, ticked
#      every ~300 ms. The point is to feel alive, not theatrical.
#      Movement is one or two characters, never a full redraw.
#
# Severity ordering (`MASCOT_SEVERITY`) tracks the worst face hit
# during a run; that peak is what survives onto the run record as
# `peak_mascot` and feeds the panic / desperation achievements.
#
# All non-ASCII characters (faces, heartbeat frames) live in
# inst/ascii/ so this source file remains ASCII-only and CRAN-portable.

# -- State machine ----------------------------------------------------

#' Compute the mascot emotional state for a run snapshot.
#'
#' Walks the progress ladder regardless of `best_p` so the player sees
#' the full emotional arc unfold during a run. The `resolved` face is
#' reserved for the final frame after a shippable run -- it isn't
#' returned mid-flight from this function. `best_p` is kept on the
#' signature for back-compat callers but no longer short-circuits the
#' ladder.
#'
#' @param progress Numeric fraction of the run budget used (0 to 1).
#' @param best_p   Numeric smallest p-value found so far in the run,
#'   or `NA` if none. Currently unused; retained for back-compat.
#' @param escalating Logical: is the derived-metric escalation phase
#'   currently active.
#' @return One of `"composed"`, `"uncertain"`, `"worried"`, `"anxious"`,
#'   `"panicked"`, `"desperate"`.
#' @keywords internal
mascot_state <- function(progress, best_p = NA_real_, escalating = FALSE) {
  if (isTRUE(escalating)) return("desperate")
  if (progress >= 0.90) return("desperate")
  if (progress >= 0.75) return("panicked")
  if (progress >= 0.55) return("anxious")
  if (progress >= 0.40) return("worried")
  if (progress >= 0.20) return("uncertain")
  "composed"
}

# Severity ordering used to track the peak emotional state of a run.
# "resolved" is the publishable outcome and isn't an emotional peak,
# so it's not on the ladder.
MASCOT_SEVERITY <- c(composed = 0L, uncertain = 1L, worried = 2L,
                     anxious = 3L, panicked = 4L, desperate = 5L)

mascot_severity <- function(state) {
  v <- MASCOT_SEVERITY[state]
  if (is.na(v)) 0L else as.integer(v)
}

severity_to_state <- function(sev) {
  names(MASCOT_SEVERITY)[match(sev, MASCOT_SEVERITY)] %||% "composed"
}

# -- Heartbeat --------------------------------------------------------

read_heartbeat_frames <- function() {
  if (!is.null(.tx$heartbeat)) return(.tx$heartbeat)
  f <- system.file("ascii", "heartbeat.txt", package = "texanshootR")
  frames <- if (nzchar(f) && file.exists(f)) {
    Filter(nzchar, readLines(f, warn = FALSE))
  } else {
    rep("...", 4L)
  }
  .tx$heartbeat <- frames
  frames
}

# 8-frame heartbeat used by the ANSI multi-zone TUI, where each zone
# (shooter, blip, modifiers, outputs, bar) gets its own row. The mascot
# row's width drift doesn't ripple into adjacent zones, so the full
# 8-frame variation in heartbeat.txt is safe there.
heartbeat_frame <- function(tick) {
  fs <- read_heartbeat_frames()
  fs[(tick %% length(fs)) + 1L]
}

# 2-frame heartbeat used by the dynamic single-line TUI. RStudio Console
# renders U+4E00 (CJK ONE) at a different cell width than `cli::ansi_nchar`
# claims (likely 1 cell because the default RStudio font lacks the glyph
# and falls back). The original 8-frame heartbeat cycles between frames
# that include U+4E00 (6 frames) and frames that don't (`-`, U+2014 em-dash
# substitutes in frames 3 and 6). Each transition between those two
# compositions shifted the mascot slot by 1 cell in RStudio, jittering
# the `|` separators tick-over-tick.
#
# The fix: in dynamic mode, keep the wide-char prefix (gun barrel glyphs
# U+FE3B U+30C7 U+2550 U+4E00) IDENTICAL on every tick, and animate only
# the muzzle dot (U+00B7 toggled with space) at the end. Whatever cell
# width RStudio renders the prefix at, it renders it the same on every
# tick, so the slot stays anchored. U+00B7 and space are both 1 cell in
# every terminal we have seen, so the dot blinking does not change the
# slot width either.
heartbeat_frame_dyn <- function(tick) {
  fs <- read_heartbeat_frames()
  base <- fs[1L]
  if ((as.integer(tick) %% 2L) == 0L) {
    base
  } else {
    sub("\u00b7$", " ", base)  # trailing U+00B7 -> space
  }
}

# Terminal cell widths for the mascot face glyphs and the dynamic-mode
# gun frame. R's `nchar(type = "width")` returns inconsistent values
# for box-drawing (U+2550, U+2014), CJK (U+4E00, U+FE3B, U+30C7), and
# Devanagari / Kannada (U+0CA0) glyphs on Windows, so we use a
# hand-verified lookup instead. The dynamic-mode mascot slot pads to
# DYN_MASCOT_WIDTH cells using these tables.
FACE_CELL_WIDTH <- c(
  composed   = 6L,   # ( o_o)
  uncertain  = 7L,   # ( -_- )
  worried    = 7L,   # ( o_o;)
  anxious    = 7L,   # ( U+2022 _ U+2022 ;)  - bullet renders 1 cell
  panicked   = 6L,   # (>_<;)
  desperate  = 5L,   # (U+0CA0 _ U+0CA0)
  resolved   = 6L    # ( -_-)
)

# Constant width: in dynamic mode the gun is always U+FE3B U+30C7 U+2550
# U+4E00 + 2 spaces + (U+00B7 or space) = 10 cells. Tick is ignored on
# this signature so callers don't need to special-case across modes.
gun_frame_cell_width <- function(tick) 10L

face_cell_width <- function(state) {
  w <- FACE_CELL_WIDTH[state]
  if (is.na(w)) {
    fallback <- tryCatch(nchar(read_face(state), type = "width"),
                          error = function(e) nchar(read_face(state)))
    return(as.integer(fallback))
  }
  as.integer(w)
}

# -- Frame loading ---------------------------------------------------

read_face <- function(state) {
  cache <- .tx$mascot_frames %||% list()
  if (!is.null(cache[[state]])) return(cache[[state]])
  f <- system.file("ascii", "mascot_frames", paste0(state, ".txt"),
                   package = "texanshootR")
  txt <- if (nzchar(f) && file.exists(f)) {
    sub("\\s+$", "", readLines(f, warn = FALSE)[1])
  } else {
    # ASCII-clean fallbacks used during dev when inst/ isn't installed.
    switch(state,
      composed   = "( o_o)",
      uncertain  = "( -_- )",
      worried    = "( o_o;)",
      anxious    = "( o_o;)",
      panicked   = "(>_<;)",
      desperate  = "( O_O )",
      resolved   = "( -_-)",
      polishing  = "( ._. )",
      submitting = "( o_o )?",
      granted    = "( -_- )b",
      rejected   = "( x_x )",
      "( o_o)")
  }
  cache[[state]] <- txt
  .tx$mascot_frames <- cache
  txt
}

read_body_template <- function() {
  if (!is.null(.tx$mascot_body)) return(.tx$mascot_body)
  f <- system.file("ascii", "mascot_frames", "body.txt", package = "texanshootR")
  body <- if (nzchar(f) && file.exists(f)) {
    readLines(f, warn = FALSE)
  } else {
    "{FACE} {GUN}"
  }
  .tx$mascot_body <- body
  body
}

# -- Cosmetic overlays -----------------------------------------------

apply_cosmetics <- function(lines, cosmetics) {
  if (is.null(cosmetics)) return(lines)
  registry <- load_cosmetic_registry()

  # The face line is whichever line contains the rendered face. Locate
  # it by finding a line wider than the bare {FACE} placeholder; in the
  # current minimalist body that's the only line.
  face_idx <- which(grepl("[(<{\\[]", lines, perl = TRUE))[1]
  if (is.na(face_idx)) face_idx <- length(lines)

  hat <- cosmetics$equipped$hat
  if (!is.na(hat)) {
    spec <- registry[registry$id == hat & registry$slot == "hat", ]
    if (nrow(spec) == 1L) {
      lines <- c(spec$overlay[[1]], lines)
      face_idx <- face_idx + length(spec$overlay[[1]])
    }
  }

  badge <- cosmetics$equipped$badge
  if (!is.na(badge)) {
    spec <- registry[registry$id == badge & registry$slot == "badge", ]
    if (nrow(spec) == 1L) {
      lines[face_idx] <- paste0(lines[face_idx], " ", spec$overlay[[1]][1])
    }
  }

  cloak <- cosmetics$equipped$cloak
  if (!is.na(cloak)) {
    spec <- registry[registry$id == cloak & registry$slot == "cloak", ]
    if (nrow(spec) == 1L) {
      lines <- c(lines, spec$overlay[[1]][1])
    }
  }

  poncho <- cosmetics$equipped$poncho
  if (!is.na(poncho)) {
    spec <- registry[registry$id == poncho & registry$slot == "poncho", ]
    if (nrow(spec) == 1L) {
      lines <- c(lines, spec$overlay[[1]][1])
    }
  }

  lanyard <- cosmetics$equipped$lanyard
  if (!is.na(lanyard)) {
    spec <- registry[registry$id == lanyard & registry$slot == "lanyard", ]
    if (nrow(spec) == 1L) {
      lines[face_idx] <- paste0(lines[face_idx], " ", spec$overlay[[1]][1])
    }
  }

  lines
}

# -- Public composer -------------------------------------------------

#' Render the mascot for a given state and heartbeat tick.
#'
#' @param state One of the five mascot states.
#' @param tick  Integer tick counter for the heartbeat animation.
#' @param cosmetics Optional wardrobe state list (slot -> equipped id).
#' @return Character vector of lines.
#' @keywords internal
mascot_render <- function(state, tick = 0L, cosmetics = NULL) {
  body <- read_body_template()
  face <- read_face(state)
  gun  <- heartbeat_frame(tick)
  out  <- vapply(body, function(line) {
    line <- gsub("{FACE}", face, line, fixed = TRUE)
    line <- gsub("{GUN}",  gun,  line, fixed = TRUE)
    line
  }, character(1), USE.NAMES = FALSE)
  apply_cosmetics(out, cosmetics)
}
