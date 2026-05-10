test_that("career() prints without erroring on a fresh save", {
  texanshootR:::ensure_save_dir()
  c1 <- career()
  expect_s3_class(c1, "tx_career")
  expect_true(nzchar(format(c1)))
  expect_output(print(c1), "Career:")
})

test_that("achievements() returns the registry with hidden labels", {
  a <- achievements()
  expect_s3_class(a, "data.frame")
  expect_true(nrow(a) >= 20L)
  expect_true(any(a$name == "???"))
})

test_that("wardrobe() list view does not error", {
  expect_invisible(wardrobe())
})
