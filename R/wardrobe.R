#' Inspect or modify the mascot wardrobe
#'
#' Without arguments, prints the equipped + unlocked cosmetics. With
#' `slot` and `id`, equips the given cosmetic in the given slot.
#'
#' @param slot Optional slot name: hat, poncho, badge, lanyard, cloak.
#' @param id   Optional cosmetic id.
#' @return The wardrobe state (invisible when modifying, visible when
#'   listing).
#' @export
wardrobe <- function(slot = NULL, id = NULL) {
  reg <- load_cosmetic_registry()
  state <- read_wardrobe_state()
  if (is.null(slot)) {
    print_wardrobe(state, reg)
    return(invisible(state))
  }
  slot <- match.arg(slot, c("hat", "poncho", "badge", "lanyard", "cloak"))
  if (is.null(id)) {
    state$equipped[[slot]] <- NA
    write_wardrobe_state(state)
    return(invisible(state))
  }
  if (!(id %in% reg$id[reg$slot == slot])) {
    stop("Unknown cosmetic '", id, "' for slot '", slot, "'.", call. = FALSE)
  }
  if (!(id %in% state$unlocked)) {
    stop("Cosmetic '", id, "' is locked.", call. = FALSE)
  }
  state$equipped[[slot]] <- id
  write_wardrobe_state(state)
  invisible(state)
}

print_wardrobe <- function(state, reg) {
  rule <- strrep("-", 48)
  cat(style_header(rule), "\n", sep = "")
  cat(style_header("Wardrobe"), "\n", sep = "")
  cat(style_header(rule), "\n", sep = "")
  for (sl in c("hat", "poncho", "badge", "lanyard", "cloak")) {
    eq <- state$equipped[[sl]]
    cat(sprintf("%-9s %s\n", paste0(sl, ":"),
                if (is.na(eq)) "-" else eq))
  }
  cat("\nUnlocked: ",
      if (length(state$unlocked) == 0L) "(none)"
      else paste(state$unlocked, collapse = ", "),
      "\n", sep = "")
}

# Auto-equip a newly-unlocked cosmetic in its slot, replacing whatever
# was equipped there before. Called from the achievement-award path.
auto_equip <- function(cosmetic_id) {
  reg <- load_cosmetic_registry()
  spec <- reg[reg$id == cosmetic_id, ]
  if (nrow(spec) != 1L) return(invisible(NULL))
  state <- read_wardrobe_state()
  if (!(cosmetic_id %in% state$unlocked)) {
    state$unlocked <- c(state$unlocked, cosmetic_id)
  }
  state$equipped[[spec$slot]] <- cosmetic_id
  write_wardrobe_state(state)
  invisible(state)
}

# Lazy registry loader. Reads inst/cosmetics/*.yaml.
load_cosmetic_registry <- function(force = FALSE) {
  if (!force && !is.null(.tx$cosmetics)) return(.tx$cosmetics)
  dir <- system.file("cosmetics", package = "texanshootR")
  if (!nzchar(dir) || !dir.exists(dir)) {
    .tx$cosmetics <- empty_cosmetic_registry()
    return(.tx$cosmetics)
  }
  files <- list.files(dir, pattern = "\\.ya?ml$", full.names = TRUE)
  rows <- list()
  for (f in files) {
    entries <- yaml::read_yaml(f)
    if (is.null(entries)) next
    for (e in entries) {
      rows[[length(rows) + 1L]] <- list(
        id      = as.character(e$id),
        slot    = as.character(e$slot),
        overlay = as.list(strsplit(e$ascii_overlay %||% "", "\n", fixed = TRUE)[[1]]),
        unlock_via = as.character(e$unlock_via %||% NA_character_)
      )
    }
  }
  if (length(rows) == 0L) {
    out <- empty_cosmetic_registry()
  } else {
    out <- data.frame(
      id         = vapply(rows, `[[`, "", "id"),
      slot       = vapply(rows, `[[`, "", "slot"),
      unlock_via = vapply(rows, `[[`, NA_character_, "unlock_via"),
      stringsAsFactors = FALSE
    )
    out$overlay <- I(lapply(rows, `[[`, "overlay"))
  }
  .tx$cosmetics <- out
  out
}

empty_cosmetic_registry <- function() {
  out <- data.frame(
    id = character(), slot = character(), unlock_via = character(),
    stringsAsFactors = FALSE
  )
  out$overlay <- I(list())
  out
}
