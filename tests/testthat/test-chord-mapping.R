context("chord mapping")

test_that("chord mapping returns as expected", {
  expect_equal(chord_is_known("a b_,fb_d'f'"), c(FALSE, TRUE))

  expect_equal(chord_name_root("a aM b_,m7#5"), c("a", "a", "b_,"))
  expect_equal(chord_name_mod("a aM b_,m7#5"), c("M", "M", "m7#5"))

  d <- gc_info("a", ignore_octave = FALSE)
  expect_is(d, "tbl_df")
  expect_equal(dim(d), c(3, 12))
  d <- gc_info("ceg a#m7_5", key = "g")
  expect_true(all(d$lp_name %in% c("a#:m7_5", "a#,:m7_5")))
  d <- gc_info("ceg a#m7_5 cM6add9", ignore_octave = FALSE)
  expect_equal(nrow(d), 7)
  expect_true(all(d$id %in% c("m7_5", "6add9")))
  expect_equal(nrow(gc_info("a,m c d f,")), 23)
  expect_equal(nrow(gc_info("a,m c d f,", ignore_octave = FALSE)), 13)
  x <- gc_fretboard("a,m c d f,", min_fret = 0:1)
  expect_equal(names(x), c("a,:m", "c:5", "d:5", "f,:5"))
  x <- list(
    gc_notes("a, b,", root_fret = 0:2, key = "g", ignore_octave = FALSE),
    gc_notes(c("a,", "b,"), root_fret = 0:2, key = "g", ignore_octave = FALSE)
  )
  expect_identical(x, list(
    as_noteworthy("a,eac#'e' b,f#bd#'f#'"),
    as_noteworthy(c("a,eac#'e'", "b,f#bd#'f#'"))
  ))
  x <- gc_notes("a, b,", root_octave =  2, key = "g", ignore_octave = FALSE)
  expect_identical(
    as.character(x),
    "a,eac#'e' a,eac#'e'a' a,c#eac#'a' b,f#bd#'f#' b,f#bd#'f#'b' b,d#f#bd#'b'")

  expect_equal(nrow(gc_info("a", bass_string = 4)), 1)
  expect_equal(nrow(gc_info("a,m", open = TRUE)), 1)
})

test_that("lp_chord_name and lp_chord_mod return as expected", {
  expect_equal(lp_chord_id("a a a", "m M m7_5"), c("a:m", "a:5", "a:m7_5"))
  expect_equal(lp_chord_mod("a a a", "m M m7_5"), c("m", "5", "m7_5"))
  expect_equal(lp_chord_id("a a a", "m M m7_5", exact = TRUE),
               c("a:m", "a:5", "a:m7es5"))
  expect_equal(lp_chord_mod("a a a", "m M m7_5", exact = TRUE),
               c("m", "5", "m7es5"))

  expect_equal(lp_chord_id("a a a a", "mb5 m7b5 m#5 m7#5"),
               c("a:m5-", "a:m7.5-", "a:m5+", "a:m7.5+"))
  expect_equal(lp_chord_id("a", "M/2"), "a:maj/b")
  expect_equal(lp_chord_id("a", "mM7"), "a:m7+")
})

test_that("chord_def returns as expected", {
  frets <- c(NA, 0, 2, 2, 1, 0)
  x1 <- chord_def(frets, "M")
  x2 <- chord_def(frets, "M", 6)
  expect_true(is.na(x1$optional))
  expect_equal(x2$optional, "e'")
  x3 <- purrr::map_dfr(c(0, 2, 3), ~chord_def(frets + .x, "M"))
  expect_equal(x3$lp_name, c("a,:5", "b,:5", "c:5"))
  frets <- c(NA, 9, 11, 11, 10, 9)
  x1 <- chord_def(frets, "M")
  x2 <- chord_def(frets, "M", 6)
  expect_true(is.na(x1$optional))
  expect_equal(x2$optional, "d_''")
})
