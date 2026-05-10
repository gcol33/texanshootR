test_that("mascot_state respects p<=0.05 and escalation", {
  expect_equal(texanshootR:::mascot_state(0.10, NA), "composed")
  expect_equal(texanshootR:::mascot_state(0.50, NA), "uncertain")
  expect_equal(texanshootR:::mascot_state(0.80, NA), "anxious")
  expect_equal(texanshootR:::mascot_state(0.99, NA), "desperate")
  expect_equal(texanshootR:::mascot_state(0.99, 0.04), "resolved")
  expect_equal(texanshootR:::mascot_state(0.10, NA, escalating = TRUE),
               "desperate")
})

test_that("mascot_render produces lines for every state", {
  for (s in c("composed", "uncertain", "anxious", "desperate", "resolved")) {
    out <- texanshootR:::mascot_render(s, tick = 0L)
    expect_type(out, "character")
    expect_gt(length(out), 1L)
  }
})

test_that("heartbeat cycles", {
  f0 <- texanshootR:::heartbeat_frame(0L)
  f1 <- texanshootR:::heartbeat_frame(1L)
  expect_false(identical(f0, f1))
})
