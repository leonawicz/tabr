context("pitch")

test_that("pitch_seq returns as expected", {
  expect_equal(pitch_seq("a,", 13),
               as_noteworthy("a, b_, b, c d_ d e_ e f g_ g a_ a"))
  expect_equal(pitch_seq("c5", -2), as_noteworthy("c5 b4"))
  expect_equal(pitch_seq("c", "b"),
               as_noteworthy("c d_ d e_ e f g_ g a_ a b_ b"))
  x <- pitch_seq("c", 8, key = "c")
  expect_equal(x, as_noteworthy("c d e f g a b c'"))
  expect_equal(pitch_seq("c", 8, "am"), x)
  expect_equal(
    pitch_seq("c#,", "a#'", "am"),
    as_noteworthy("d, e, f, g, a, b, c d e f g a b c' d' e' f' g' a'")
  )
  expect_equal(pitch_seq("a", 8, "am", "harmonic minor"),
               as_noteworthy("a b c' d' e' f' g#' a'"))
  expect_equal(pitch_seq("a'", -2), as_noteworthy("a' a_'"))

  expect_error(pitch_seq(c("a", "b")), "`x` must be a single pitch.")
  expect_error(pitch_seq("a", 0), "Cannot have zero timesteps.")
  expect_error(pitch_seq("a", 1:2),
               "`y` must be a single pitch or single number.")
  expect_error(pitch_seq("b,,,,,", 1),
               "Pitch semitones must range from 0 to 131.")
  expect_error(pitch_seq("a'", -71), "Semitones must range from 0 to 131.")
  expect_error(pitch_seq("a'", "b,,,,,"),
               "Pitch semitones must range from 0 to 131.")
})
