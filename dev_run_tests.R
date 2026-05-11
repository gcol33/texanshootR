suppressPackageStartupMessages(devtools::load_all(quiet = TRUE))
res <- as.data.frame(testthat::test_local(reporter = "silent",
                                          stop_on_failure = FALSE))
cat(sprintf("files=%d  PASS=%d  FAIL=%d  WARN=%d  SKIP=%d  ERROR=%d\n",
            length(unique(res$file)),
            sum(res$nb), sum(res$failed),
            sum(res$warning), sum(res$skipped),
            sum(res$error)))
if (any(res$failed > 0 | res$error)) {
  cat("\nFailing/erroring tests:\n")
  bad <- res[res$failed > 0 | res$error, c("file", "context", "test", "failed", "error")]
  print(bad, row.names = FALSE)
}
