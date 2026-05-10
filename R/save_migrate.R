# Save schema migrators. Each migrate_<from>_to_<to> function takes a
# parsed meta object at version `<from>` and returns one at `<to>`.
# `migrate_meta` chains them.

migrate_meta <- function(meta) {
  v <- meta$save_version %||% 0L
  while (v < SAVE_VERSION) {
    fn <- get0(sprintf("migrate_%d_to_%d", v, v + 1L), inherits = TRUE)
    if (is.null(fn)) {
      stop("No migrator from save_version ", v, " to ", v + 1L, call. = FALSE)
    }
    meta <- fn(meta)
    v <- v + 1L
  }
  meta$save_version <- SAVE_VERSION
  meta
}

# Example placeholder for future schema versions:
# migrate_1_to_2 <- function(meta) {
#   meta$some_new_field <- list()
#   meta
# }
