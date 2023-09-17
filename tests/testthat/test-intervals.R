test_that("interval helpers return as expected", {
  expect_equal(pitch_interval("c d e", "d3 d d"), c(2, 0, -2))
  expect_equal(pitch_interval("c2", "d"), 14)
  expect_equal(pitch_interval("b,", "c,"), -11)
  expect_equal(pitch_interval("c#,", "b,"), 10)
  expect_equal(pitch_interval("b_,", "c"), 2)
  expect_error(pitch_interval("b_,", "c d"),
               "Inputs must have equal number of timesteps.")

  expect_equal(pitch_interval("c", "d3"), 2)
  expect_equal(pitch_interval("d5", "c,"), -38)
  expect_equal(pitch_interval("b,", "c3"), 1)
  expect_equal(pitch_interval("c", "b4"), 23)

  expect_identical(scale_interval(NA, "c"), NA_character_)
  expect_equal(scale_interval("c d# e", "d3 d d'"), c("M2", "m2", "m7"))
  expect_equal(scale_interval("c2", "d_"), "m9")
  expect_equal(scale_interval("e,", "b,"), "P5")
  expect_equal(scale_interval("e,", "b"), "P12")
  expect_equal(scale_interval("e,", "b_"), NA_character_)
  expect_equal(scale_interval("e,", "b_", format = "ad"),
               "diminished twelfth/augmented eleventh")
  expect_equal(scale_interval("e'", "c#'''"), "M13")
  expect_equal(scale_interval("c#", "e_"), "M2")
  expect_equal(scale_interval("d_", "e_"), "M2")
  expect_identical(scale_interval("a", "c'''"), NA_character_)

  x <- "a, b, c d e f g# ac'e' a c' e'"
  expect_equal(pitch_diff(x), c(NA, 2, 1, 2, 2, 1, 3, 1, 0, 3, 4))
  expect_equal(pitch_diff(x, use_root = FALSE),
               c(NA, 2, 1, 2, 2, 1, 3, NA, NA, 3, 4))
  expect_identical(
    scale_diff(x),
    c(NA, "M2", "m2", "M2", "M2", "m2", "m3", "m2", "P1", "m3", "M3"))
  expect_identical(
    scale_diff(x, n = 2, trim = TRUE, use_root = FALSE),
    c("m3", "m3", "M3", "m3", "M3", NA, "m2", NA, "P5"))

  x <- "a, c r r r r g"
  expect_equal(pitch_diff(x), c(NA, 3, NA, NA, NA, NA, 7))
  expect_identical(scale_diff(x), c(NA, "m3", NA, NA, NA, NA, "P5"))
  expect_equal(pitch_diff(x, n = 2), c(rep(NA, 6), 10))
  expect_identical(scale_diff(x, n = 2), c(rep(NA, 6), "m7"))
  expect_equal(pitch_diff(x, n = 2, trim = TRUE), c(rep(NA, 4), 10))
  expect_identical(scale_diff(x, n = 2, trim = TRUE), c(rep(NA, 4), "m7"))

  expect_equal(tuning_intervals(), c(0, 5, 10, 15, 19, 24))
})
