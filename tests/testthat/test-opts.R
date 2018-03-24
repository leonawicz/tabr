context("options")

test_that("options set on package load", {
  x <- tabr_options()
  expect_is(x, "list")
  expect_identical(names(x), c("dev", "midi", "lilypond", "midi2ly", "python"))
  expect_is(tabr_options(x = 1), "NULL")
  expect_is(tabr_options(dev = "png"), "NULL")
  expect_equal(tabr_options()$x, 1)
  expect_equal(tabr_options()$dev, "png")
})
