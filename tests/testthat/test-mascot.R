test_that("mascot_state walks the progress ladder regardless of p", {
  expect_equal(texanshootR:::mascot_state(0.10, NA), "composed")
  expect_equal(texanshootR:::mascot_state(0.30, NA), "uncertain")
  expect_equal(texanshootR:::mascot_state(0.50, NA), "worried")
  expect_equal(texanshootR:::mascot_state(0.65, NA), "anxious")
  expect_equal(texanshootR:::mascot_state(0.80, NA), "panicked")
  expect_equal(texanshootR:::mascot_state(0.99, NA), "desperate")
  # best_p no longer short-circuits the ladder -- "resolved" is now
  # only set at end-of-run by shoot() when the run is shippable.
  expect_equal(texanshootR:::mascot_state(0.99, 0.04), "desperate")
  expect_equal(texanshootR:::mascot_state(0.10, 0.04), "composed")
  expect_equal(texanshootR:::mascot_state(0.10, NA, escalating = TRUE),
               "desperate")
})

test_that("mascot_render produces a face line for every state", {
  for (s in c("composed", "uncertain", "worried", "anxious",
              "panicked", "desperate", "resolved")) {
    out <- texanshootR:::mascot_render(s, tick = 0L)
    expect_type(out, "character")
    expect_gte(length(out), 1L)
    expect_match(out[1], "[(<{\\[]", perl = TRUE)
  }
})

test_that("ANSI-mode heartbeat cycles through frames", {
  # ANSI multi-zone mode gets the full 8-frame heartbeat because each
  # zone has its own row, so the mascot row's width drift cannot ripple
  # into adjacent zones.
  f0 <- texanshootR:::heartbeat_frame(0L)
  f1 <- texanshootR:::heartbeat_frame(1L)
  expect_false(identical(f0, f1))
})

test_that("dynamic-mode heartbeat keeps the wide-char prefix constant", {
  # Dynamic single-line mode toggles only the trailing muzzle dot (`·`
  # vs space), keeping `︻デ═一` identical across ticks. This is
  # because the slot shares a row with loading text and the bar, and
  # any cell-width disagreement between the standard and RStudio's
  # rendering of `一` would jitter the `|` separators tick-over-tick.
  f0 <- texanshootR:::heartbeat_frame_dyn(0L)
  f1 <- texanshootR:::heartbeat_frame_dyn(1L)
  f2 <- texanshootR:::heartbeat_frame_dyn(2L)
  # Both frames must contain the constant prefix `︻デ═一` -- if a
  # future edit reintroduces `一` ↔ `-`/`—` cycling in dyn mode, this
  # catches it.
  prefix <- intToUtf8(c(0xFE3B, 0x30C7, 0x2550, 0x4E00))  # ︻デ═一
  expect_true(grepl(prefix, f0, fixed = TRUE))
  expect_true(grepl(prefix, f1, fixed = TRUE))
  # Same wide-char prefix, only trailing detail varies between ticks.
  expect_false(identical(f0, f1))
  expect_identical(f0, f2)
  expect_identical(nchar(f0), nchar(f1))
})
