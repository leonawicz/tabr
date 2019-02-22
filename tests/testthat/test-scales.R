context("scales")

test_that("scales return as expected", {
  expect_equal(scale_diatonic("c", collapse = TRUE), "c d e f g a b")
  expect_equal(scale_diatonic("am", collapse = TRUE), "a2 b2 c d e f g")
  expect_equal(length(scale_chromatic("c")), 12)

  expect_equal(scale_diatonic(key = "dm", TRUE), "d e f g a b_ c4")
  expect_equal(scale_minor(key = "dm", TRUE, TRUE), "d e f g a b_ c")
  expect_equal(scale_major(key = "d", TRUE), "d e f# g a b c#4")

  expect_equal(scale_chromatic(root = "a", ignore_octave = TRUE),
               c("a",  "a#", "b",  "c",  "c#", "d",  "d#", "e",  "f",  "f#", "g",  "g#"))

  expect_equal(scale_harmonic_minor("am", TRUE), "a2 b2 c d e f g#")
  expect_equal(scale_hungarian_minor("am", TRUE, TRUE), "a b c d# e f g#")

  expect_identical(scale_melodic_minor("am"), scale_jazz_minor("am"))
  expect_equal(rev(scale_melodic_minor("am", descending = TRUE, ignore_octave = TRUE)), letters[1:7])
  expect_equal(scale_jazz_minor("am", collapse = TRUE), "a2 b2 c d e f# g#")

  expect_equal(length(modes()), 7)
  expect_equal(length(modes("major")), 3)
  expect_equal(length(modes("minor")), 4)
})

test_that("other scale helpers return as expected", {
  expect_equal(scale_degree("c e g"), c(1, 3, 5))
  expect_equal(scale_degree("c e g", roman = TRUE), as.roman(c(1, 3, 5)))
  expect_equal(scale_degree("c e g", key = "d"), c(NA, 2, 4))

  x <- "c, e_3 g' f#ac#"
  expect_equal(scale_degree(x), c(1, NA, 5, NA))
  expect_equal(scale_degree(x, naturalize = TRUE), c(1, 3, 5, 4))
  expect_equal(scale_degree(x, scale = "chromatic"), c(1, NA, 8, 7))
  expect_equal(scale_degree(x, scale = "chromatic", sharp = FALSE), c(1, 4, 8, NA))

  expect_equal(scale_note(1:3), c("c", "d", "e"))
  expect_equal(scale_note(c(1, 3, 8), "d", collapse = TRUE), "d f# NA")
  expect_true(all(sapply(list(4, "IV", as.roman(4)), scale_note) == "f"))

  expect_equal(chord_is_diatonic("f#ac# f#ac", "a"), c(TRUE, FALSE)) # nolint

  err <- "`tabr::scale_a` is not an exported scale."
  expect_error(scale_degree(x, scale = "a"), err)
  expect_error(scale_note(1, scale = "a"), err)
  expect_error(scale_note(0), "`deg` should be >= 1.")
  expect_error(chord_is_diatonic("ceg x"), "Invalid chord found.")
})

test_that("scale_chords returns as expected", {
  expect_equal(scale_chords("c", "major", collapse = TRUE), "ceg dfa egb fac4 gbd4 ac4e4 bd4f4")
  expect_equal(scale_chords("a", "minor", collapse = TRUE), "a2ce b2df ceg dfa egb fac4 gbd4")
  expect_equal(scale_chords("a", "harmonic minor", collapse = TRUE), "a2ce b2df ceg# dfa eg#b fac4 g#bd4")
  expect_identical(scale_chords("a", "melodic minor"), scale_chords("a", "jazz minor"))
  expect_equal(scale_chords("a", "hungarian minor"),
               c("a2ce", "b2d#f", "ceg#", "d#fa", "eg#b", "fac4", "g#bd#4"))

  expect_equal(scale_chords("c", "major", "seventh", collapse = TRUE),
               "cegb dfac4 egbd4 fac4e4 gbd4f4 ac4e4g4 bd4f4a4")
  expect_equal(scale_chords("a", "minor", "seventh", collapse = TRUE),
               "a2ceg b2dfa cegb dfac4 egbd4 fac4e4 gbd4f4")
})
