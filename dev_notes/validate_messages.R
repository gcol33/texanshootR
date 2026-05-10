suppressPackageStartupMessages({
  devtools::load_all(".", quiet = TRUE)
})

reg <- validate_messages()
cat(sprintf("Registry size: %d messages\n", nrow(reg)))
cat("\nBy phase:\n")
print(table(reg$trigger_phase))
cat("\nBy rarity:\n")
print(table(reg$rarity))
cat("\nBy fallacy tag (top 23):\n")
fallacy_tags <- vocab_tags[seq_len(23)]
ftab <- vapply(fallacy_tags, function(t) sum(vapply(reg$tags, function(x) t %in% x, logical(1))), integer(1))
print(sort(ftab, decreasing = TRUE))
cat("\nValidation OK.\n")
