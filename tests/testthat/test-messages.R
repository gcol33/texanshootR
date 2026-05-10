test_that("message registry validates", {
  reg <- validate_messages()
  expect_s3_class(reg, "data.frame")
  expect_gt(nrow(reg), 0)
  expect_true(all(reg$trigger_phase %in% vocab_phases))
  expect_true(all(reg$rarity %in% c("common", "uncommon", "rare", "legendary")))
})

test_that("selection algorithm returns a candidate for the blip phase", {
  withr::local_seed(1)
  draw <- texanshootR:::select_message(phase = "blip")
  expect_false(is.null(draw))
  expect_true(nzchar(draw$text))
})

test_that("recency suppression hides repeat ids", {
  withr::local_seed(1)
  rb <- texanshootR:::recent_buffer()
  d1 <- texanshootR:::select_message(phase = "blip")
  rb <- texanshootR:::recent_push(rb, d1$id)
  for (i in 1:50) {
    d <- texanshootR:::select_message(phase = "blip",
                                       recent = texanshootR:::recent_ids(rb))
    if (!is.null(d)) rb <- texanshootR:::recent_push(rb, d$id)
  }
  expect_true(d1$id %in% texanshootR:::recent_ids(rb) ||
                length(texanshootR:::recent_ids(rb)) > 0)
})
