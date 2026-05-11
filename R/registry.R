# Shared registry loader. Three on-disk registries (achievements, life
# events, wardrobe cosmetics) all follow the same pattern: lazy-cache
# under .tx, read inst/<dir>/*.yaml, flatten entries to rows, build a
# data.frame. The variation is the per-entry shape and the final frame
# (some carry I()-wrapped list columns). Those go in via the row +
# frame callbacks.

load_registry <- function(inst_dir, cache_key,
                          entry_to_row, rows_to_frame, empty,
                          force = FALSE) {
  if (!force && !is.null(.tx[[cache_key]])) return(.tx[[cache_key]])
  dir <- system.file(inst_dir, package = "texanshootR")
  if (!nzchar(dir) || !dir.exists(dir)) {
    .tx[[cache_key]] <- empty()
    return(.tx[[cache_key]])
  }
  files <- list.files(dir, pattern = "\\.ya?ml$", full.names = TRUE)
  rows <- list()
  for (f in files) {
    entries <- yaml::read_yaml(f)
    if (is.null(entries)) next
    for (e in entries) {
      rows[[length(rows) + 1L]] <- entry_to_row(e)
    }
  }
  out <- if (length(rows) == 0L) empty() else rows_to_frame(rows)
  .tx[[cache_key]] <- out
  out
}

# Build an empty data.frame whose character columns are listed in
# `char_cols`, with one trailing list-column attached via I(list()).
# Used by the event and cosmetic empty-registry constructors.
new_list_col_frame <- function(char_cols, list_col) {
  args <- stats::setNames(
    replicate(length(char_cols), character(), simplify = FALSE),
    char_cols
  )
  args$stringsAsFactors <- FALSE
  out <- do.call(data.frame, args)
  out[[list_col]] <- I(list())
  out
}
