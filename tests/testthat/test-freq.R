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

test_that("frequency ratios compile correctly", {
  x <- as_music("c4 e_ g c'e_'g'")
  fr <- freq_ratio(x)

  x <- music_notes(x)
  expect_identical(fr, freq_ratio(x))

  x <- chord_freq(x)
  fr1 <- freq_ratio(x)
  expect_identical(fr, fr1)

  fr2 <- freq_ratio(x, octaves = "integer", accidentals = "sharp")
  expect_identical(fr1[, -2], fr2[, -2])
  expect_equal(octave_type(fr2$notes), "integer")
  expect_equal(accidental_type(fr2$notes), "sharp")
  expect_identical(fr1$notes, as_noteworthy(fr2$notes, "tick", "flat"))

  y <- freq_ratio(x, ratios = "root")
  expect_identical(y$notes,
                   as_noteworthy("c e_ g c'e_' c'g'", format = "vector"))
  y <- freq_ratio(x, ratios = "range")
  expect_identical(y$notes, as_noteworthy("c e_ g c'g'", format = "vector"))

  x <- "c e_ g c'e_'g'"
  fr3 <- freq_ratio(x)
  expect_identical(fr, fr3)
})
