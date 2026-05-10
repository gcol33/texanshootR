# Mascot module.
#
# Two layers of animation:
#   1. EMOTIONAL STATE (slow): face changes on `mascot_state` transitions
#      driven by run progress + best p-value. Five states: composed,
#      uncertain, anxious, desperate, resolved.
#   2. HEARTBEAT (fast): tiny 8-frame cycle on the gun line, ticked
#      every ~300 ms. The point is to feel alive, not theatrical.
#      Movement is one or two characters, never a full redraw.
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
#' @return One of `"composed"`, `"uncertain"`, `"anxious"`,
#'   `"desperate"`, `"resolved"`.
#' @keywords internal
mascot_state <- function(progress, best_p = NA_real_, escalating = FALSE) {
  if (!is.na(best_p) && best_p <= 0.05) return("resolved")
  if (isTRUE(escalating)) return("desperate")
  if (progress >= 0.95) return("desperate")
  if (progress >= 0.66) return("anxious")
  if (progress >= 0.33) return("uncertain")
  "composed"
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
      anxious   = "( o_o;)",
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
    c(
      "        _____",
      "       /_____\\",
      "   ___|_______|___",
      "       {FACE} {GUN}",
      "        /| |\\",
      "        |* *|",
      "        | | |",
      "       d/   \\b"
    )
  }
  .tx$mascot_body <- body
  body
}

# -- Cosmetic overlays -----------------------------------------------

apply_cosmetics <- function(lines, cosmetics) {
  if (is.null(cosmetics)) return(lines)
  registry <- load_cosmetic_registry()

  hat <- cosmetics$equipped$hat
  if (!is.na(hat)) {
    spec <- registry[registry$id == hat & registry$slot == "hat", ]
    if (nrow(spec) == 1L) {
      lines <- c(spec$overlay[[1]], lines)
    }
  }

  badge <- cosmetics$equipped$badge
  if (!is.na(badge)) {
    spec <- registry[registry$id == badge & registry$slot == "badge", ]
    if (nrow(spec) == 1L) {
      face_idx <- 3L
      if (length(lines) >= face_idx) {
        lines[face_idx] <- paste0(lines[face_idx], " ", spec$overlay[[1]][1])
      }
    }
  }

  cloak <- cosmetics$equipped$cloak
  if (!is.na(cloak)) {
    spec <- registry[registry$id == cloak & registry$slot == "cloak", ]
    if (nrow(spec) == 1L && length(lines) >= 5L) {
      lines[length(lines)] <- spec$overlay[[1]][1]
    }
  }

  poncho <- cosmetics$equipped$poncho
  if (!is.na(poncho)) {
    spec <- registry[registry$id == poncho & registry$slot == "poncho", ]
    if (nrow(spec) == 1L && length(lines) >= 5L) {
      lines[5] <- spec$overlay[[1]][1]
    }
  }

  lanyard <- cosmetics$equipped$lanyard
  if (!is.na(lanyard)) {
    spec <- registry[registry$id == lanyard & registry$slot == "lanyard", ]
    if (nrow(spec) == 1L) {
      face_idx <- 3L
      if (length(lines) >= face_idx) {
        lines[face_idx] <- paste0(lines[face_idx], " ", spec$overlay[[1]][1])
      }
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
