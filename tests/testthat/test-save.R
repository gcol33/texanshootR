test_that("save dir is sandboxed via texanshootR.save_dir", {
  d <- getOption("texanshootR.save_dir")
  expect_true(!is.null(d))
  expect_equal(texanshootR:::save_dir(), d)
})

test_that("default meta + write/read round trip", {
  d <- texanshootR:::ensure_save_dir()
  meta <- texanshootR:::read_meta()
  expect_true(!is.null(meta))
  expect_equal(meta$save_version, texanshootR:::SAVE_VERSION)
})

test_that("reset_all wipes the save dir", {
  texanshootR:::ensure_save_dir()
  reset_all(force = TRUE)
  expect_null(texanshootR:::read_meta())
})
