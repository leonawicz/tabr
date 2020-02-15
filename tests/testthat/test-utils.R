context("utils")

test_that("Utility functions return as expected", {
  skip_on_appveyor()
  expect_message(lilypond_root())
  expect_is(lilypond_version(), "NULL")
  expect_message(tabr_lilypond_api())
})
