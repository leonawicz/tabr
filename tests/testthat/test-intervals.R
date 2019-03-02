context("intervals")

test_that("interval helpers return as expected", {
  expect_equal(pitch_interval("c", "d3"), 2)
  expect_equal(pitch_interval("c2", "d"), 14)
  expect_equal(pitch_interval("b,", "c,"), -11)
  expect_equal(pitch_interval("c#,", "b,"), 10)
  expect_equal(pitch_interval("b_,", "c"), 2)

  expect_equal(pitch_interval("c", "d3", ignore_octave = TRUE), 2)
  expect_equal(pitch_interval("d5", "c,", ignore_octave = TRUE), -2)
  expect_equal(pitch_interval("b,", "c3", ignore_octave = TRUE), -11)
  expect_equal(pitch_interval("c", "b4", ignore_octave = TRUE), 11)

  expect_equal(scale_interval("c", "d3"), "M2")
  expect_equal(scale_interval("c2", "d_"), "m9")
  expect_equal(scale_interval("e,", "b,"), "P5")
  expect_equal(scale_interval("e,", "b"), "P12")
  expect_equal(scale_interval("e,", "b", ignore_octave = TRUE), "P5")
  expect_equal(scale_interval("e'", "c#'''"), "M13")

  expect_equal(tuning_intervals(), c(0, 5, 10, 15, 19, 24))
})
