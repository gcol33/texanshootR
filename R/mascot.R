# The shooter — the package's protagonist.
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
#' @param progress Numeric fraction of the run budget used (0 to 1).
#' @param best_p   Numeric smallest p-value found so far in the run,
#'   or `NA` if none.
#' @param escalating Logical: is the derived-metric escalation phase
#'   currently active.
#' @return One of `"composed"`, `"uncertain"`, `"worried"`, `"anxious"`,
#'   `"panicked"`, `"desperate"`, `"resolved"`.
#' @keywords internal
mascot_state <- function(progress, best_p = NA_real_, escalating = FALSE) {
  if (!is.na(best_p) && best_p <= 0.05) return("resolved")
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

heartbeat_frame <- function(tick) {
  fs <- read_heartbeat_frames()
  fs[(tick %% length(fs)) + 1L]
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
      composed  = "( o_o)",
      uncertain = "( -_- )",
      worried   = "( o_o;)",
      anxious   = "( o_o;)",
      panicked  = "(>_<;)",
      desperate = "( O_O )",
      resolved  = "( -_-)",
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
