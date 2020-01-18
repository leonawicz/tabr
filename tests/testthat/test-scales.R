context("scales")

library(dplyr)

test_that("scales return as expected", {
  expect_equal(scale_diatonic("c", collapse = TRUE) %>% as.character(),
               "c d e f g a b")
  expect_equal(scale_diatonic("am", collapse = TRUE) %>% as.character(),
               "a, b, c d e f g")
  expect_equal(length(scale_chromatic("c")), 12)

  expect_equal(scale_diatonic(key = "dm", TRUE) %>% as.character(),
               "d e f g a b_ c'")
  expect_equal(scale_minor(key = "dm", TRUE, TRUE) %>% as.character(),
               "d e f g a b_ c")
  expect_equal(scale_major(key = "d", TRUE) %>% as.character(),
               "d e f# g a b c#'")

  expect_equal(scale_chromatic(root = "a", collapse = TRUE) %>% as.character(),
               c("a, a#, b, c c# d d# e f f# g g#"))
  expect_equal(scale_chromatic(root = "a", ignore_octave = TRUE) %>%
                 as.character(), c("a",  "a#", "b",  "c",  "c#", "d",  "d#",
                                   "e",  "f",  "f#", "g",  "g#"))

  expect_equal(scale_harmonic_minor("am", TRUE) %>% as.character(),
               "a, b, c d e f g#")
  expect_equal(scale_hungarian_minor("am", TRUE, TRUE) %>% as.character(),
               "a b c d# e f g#")

  expect_identical(scale_melodic_minor("am"), scale_jazz_minor("am"))
  expect_equal(
    rev(scale_melodic_minor("am", descending = TRUE, ignore_octave = TRUE)),
    as_noteworthy(letters[1:7]))
  expect_equal(scale_jazz_minor("am", collapse = TRUE) %>% as.character(),
               "a, b, c d e f# g#")

  expect_equal(length(modes()), 7)
  expect_equal(length(modes("major")), 3)
  expect_equal(length(modes("minor")), 4)

  expect_error(scale_major("a#"), "`key` does not indicate a valid major key.")
  expect_error(scale_minor("g_"), "`key` does not indicate a valid minor key.")
  expect_error(scale_chromatic("a_"),
               "`root` is not one of: c c# d d# e f f# g g# a a# b")
  expect_error(scale_chromatic("c#", sharp = FALSE),
               "`root` is not one of: c d_ d e_ e f g_ g a_ a b_ b")
})

test_that("other scale helpers return as expected", {
  x <- "r c, e3 g~ g s g# ceg"
  expect_equal(scale_degree(x), c(NA, 1, 3, 5, 5, NA, NA, 1))
  expect_equal(note_in_scale(x), c(NA, T, T, T, T, NA, F, T))
  expect_equal(scale_degree("a b c d e f g", "am", "harmonic_minor"),
               c(1:6, NA))
  expect_equal(scale_degree("c e g", roman = TRUE), as.roman(c(1, 3, 5)))
  expect_equal(scale_degree("c e g", key = "d"), c(NA, 2, 4))

  expect_equal(scale_degree("c c# d_ e", key = "d"), c(NA, 7, NA, 2))
  expect_equal(scale_degree("c c# d_ e", key = "d", strict_accidentals = FALSE),
               c(NA, 7, 7, 2))

  x <- "c, e_3 g' f#ac#"
  expect_equal(scale_degree(x), c(1, NA, 5, NA))
  expect_equal(scale_degree(x, use_root = FALSE), c(1, NA, 5, NA))
  expect_equal(scale_degree(x, naturalize = TRUE), c(1, 3, 5, 1))
  expect_equal(scale_degree(x, scale = "chromatic"), c(1, 4, 8, 2))
  expect_equal(
    scale_degree("c# d_ e_' e4 f f# g", key = "c#", scale = "chromatic"),
    c(1, 1, 3:7))

  expect_equal(scale_note(1:3), as_noteworthy(c("c", "d", "e")))
  expect_equal(scale_note(c(1, 3, 8), "d", collapse = TRUE),
               as_noteworthy("d f# d"))
  expect_true(all(sapply(list(4, "IV", as.roman(4)), scale_note) == "f"))
  expect_equal(scale_note(1:8, "e"),
               as_noteworthy("e f# g# a b c# d# e", format = "vector"))

  expect_equal(is_diatonic("f2#a3c#' r s f#ac", "a"), c(T, NA, NA, F)) # nolint

  err <- "`tabr::scale_a` is not an exported scale."
  expect_error(scale_degree(x, scale = "a"), err)
  expect_error(scale_note(1, scale = "a"), err)
  expect_error(scale_note(0), "`deg` should be >= 1.")
  expect_error(is_diatonic("ceg x"), "Invalid notes or chords found.")

  expect_identical(note_in_scale(x), c(T, F, T, FALSE))
  expect_identical(note_in_scale("c e_ g"), c(TRUE, FALSE, TRUE))
  expect_identical(note_in_scale("c e_ g", "cm", "minor"), c(TRUE, TRUE, TRUE))

  expect_equal(
    chord_degree("c#d d_ e_' e4 f f# g", key = "c#", scale = "chromatic"),
    list(1:2, 1, 3, 4, 5, 6, 7))
  x <- "r s d~ dfa df#a f#ac#"
  expect_equal(chord_degree(x, "d"),
                   list(NA_integer_, NA_integer_, 1, c(1, NA, 5), c(1, 3, 5),
                        c(3, 5, 7)))
  expect_identical(is_in_scale(x, "d"), c(NA, NA, T, F, T, T))
})

test_that("scale_chords returns as expected", {
  expect_equal(scale_chords("c", "major", collapse = TRUE),
               as_noteworthy("ceg dfa egb fac' gbd' ac'e' bd'f'"))
  expect_equal(scale_chords("a", "minor", collapse = TRUE),
               as_noteworthy("a,ce b,df ceg dfa egb fac' gbd'"))
  expect_equal(scale_chords("a", "harmonic minor", collapse = TRUE),
               as_noteworthy("a,ce b,df ceg# dfa eg#b fac' g#bd'"))
  expect_identical(scale_chords("a", "melodic minor"),
                   scale_chords("a", "jazz minor"))
  expect_equal(scale_chords("a", "hungarian minor"),
               as_noteworthy(
                 c("a,ce", "b,d#f", "ceg#", "d#fa", "eg#b", "fac'", "g#bd#'")))

  expect_equal(scale_chords("c", "major", "seventh", collapse = TRUE),
               as_noteworthy("cegb dfac' egbd' fac'e' gbd'f' ac'e'g' bd'f'a'"))
  expect_equal(scale_chords("a", "minor", "seventh", collapse = TRUE),
               as_noteworthy("a,ceg b,dfa cegb dfac' egbd' fac'e' gbd'f'"))
})
