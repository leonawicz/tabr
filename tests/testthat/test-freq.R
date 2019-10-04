context("freq")

test_that("frequency conversions return as expected", {
  x <- "a e4 a4 e5 a5"
  y <- pitch_freq(x)
  expect_true(all(y - c(220, 329.6276, 440, 659.2551, 880) < 1e-4))
  expect_identical(as_noteworthy(x), freq_pitch(y, collapse = TRUE))
  expect_error(pitch_freq(x, "h"), "Invalid `fixed_note`.")
  expect_error(pitch_freq(x, fixed_freq = 0), "Invalid `fixed_freq`.")
})
