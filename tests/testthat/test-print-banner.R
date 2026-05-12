# Sectioned print.tx_run banner: header rule, RUN/FINDING/SEARCH/OUTPUTS/
# DATA USED sections always present, PUBLICATION PIPELINE only when the
# printed run owns the active chain.

capture_banner <- function(run) {
  utils::capture.output(print(run))
}

test_that("banner header carries the run number, zero-padded to four", {
  withr::local_options(texanshootR.budget = 1)
  withr::local_seed(3)
  run <- shoot(mtcars)
  out <- capture_banner(run)
  hit <- grep("^texanshootR :: run \\d{4}$", out, value = TRUE)
  expect_length(hit, 1L)
})

test_that("the five always-on sections render in order", {
  withr::local_options(texanshootR.budget = 1)
  withr::local_seed(5)
  run <- shoot(mtcars)
  out <- capture_banner(run)
  fixed_sections <- c("RUN", "FINDING", "SEARCH", "OUTPUTS", "DATA USED")
  positions <- vapply(fixed_sections,
                      function(s) which(out == s)[1],
                      integer(1))
  expect_false(any(is.na(positions)))
  expect_equal(positions, sort(positions))
})

test_that("RUN section reports status, career impact, mascot arc", {
  withr::local_options(texanshootR.budget = 1)
  withr::local_seed(7)
  run <- shoot(mtcars)
  out <- capture_banner(run)
  expect_true(any(grepl("^status: ", out)))
  expect_true(any(grepl("^career impact: (promoted|advanced|stalled)$", out)))
  expect_true(any(grepl("^mascot state: .+ → .+$", out)))
})

test_that("OUTPUTS section uses `fn(run)` for unlocked, bare for locked", {
  withr::local_options(texanshootR.budget = 1)
  withr::local_seed(9)
  run <- shoot(mtcars)
  out <- capture_banner(run)
  # At minimum the Junior Researcher tier has abstract unlocked.
  expect_true(any(grepl("^available now: abstract\\(run\\)$", out)))
  # Locked rows read "fn()  requires <tier>".
  expect_true(any(grepl("\\(\\) +requires (Postdoc|Senior Scientist|PI)$", out)))
})

test_that("DATA USED always reports the six labeled fields", {
  withr::local_options(texanshootR.budget = 1)
  withr::local_seed(13)
  run <- shoot(mtcars)
  out <- capture_banner(run)
  for (label in c("exclusion rule:", "rows excluded:",
                  "outcome construction:",
                  "derived metrics:", "search seed:", "run stored:")) {
    expect_true(any(grepl(label, out, fixed = TRUE)),
                info = sprintf("missing DATA USED label: %s", label))
  }
})

test_that("PUBLICATION PIPELINE appears only when run owns active chain", {
  withr::local_options(texanshootR.budget = 1)
  withr::local_seed(17)
  run <- shoot(mtcars)
  out <- capture_banner(run)

  meta <- texanshootR:::read_meta()
  owns_chain <- !is.null(meta) && !is.null(meta$active_chain) &&
                identical(meta$active_chain$run_id, run$run_id)
  if (isTRUE(owns_chain)) {
    expect_true(any(out == "PUBLICATION PIPELINE"))
  } else {
    expect_false(any(out == "PUBLICATION PIPELINE"))
  }
})
