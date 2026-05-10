suppressWarnings(devtools::load_all(quiet = TRUE))
states <- c("composed", "uncertain", "worried", "anxious",
            "panicked", "desperate", "resolved")
for (s in states) {
  cat("---", s, "---\n", sep = " ")
  cat(paste(texanshootR:::mascot_render(s, tick = 0L), collapse = "\n"), "\n\n", sep = "")
}
