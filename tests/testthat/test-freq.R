context("freq")

test_that("frequency conversions return as expected", {
  x <- "a e4 a4 e5 a5"
  y <- pitch_freq(x)
  expect_true(all(y - c(220, 329.6276, 440, 659.2551, 880) < 1e-4))
  expect_identical(as_noteworthy(x), freq_pitch(y, collapse = TRUE) %>%
                     as_integer_octaves())
  expect_identical(pitch_semitones(x), as.integer(c(57, 64, 69, 76, 81)))
  expect_true(all(abs(freq_semitones(y) - c(57, 64, 69, 76, 81)) < 0.0001))

  x <- c("a,, c, e, c,e_,g, ce_gb_ a'")
  expect_error(pitch_freq(x), "Invalid note found.")

  y <- chord_freq(x)
  expect_is(y, "list")
  expect_equal(as.numeric(sapply(y, length)), c(1, 1, 1, 3, 4, 1))

  expect_equal(names(y), strsplit(x, " ")[[1]])

  y <- chord_semitones(x)
  expect_is(y, "list")
  expect_equal(length(y), 6)
  expect_equal(as.numeric(sapply(y, length)), c(1, 1, 1, 3, 4, 1))

  expect_error(semitone_pitch(-1), "Semitones must range from 0 to 131.")
})
