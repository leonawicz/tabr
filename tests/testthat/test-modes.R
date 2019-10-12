context("modes")

test_that("mode helpers return as expected", {
  expect_equal(mode_ionian(), mode_modern())
  expect_equal(mode_dorian(), mode_modern("dorian"))
  expect_equal(mode_phrygian(), mode_modern("phrygian"))
  expect_equal(mode_lydian(), mode_modern("lydian"))
  expect_equal(mode_mixolydian("b_"), mode_modern("mixolydian", "b_"))
  expect_equal(mode_aeolian("c#"), mode_modern("aeolian", "c#"))
  expect_equal(mode_locrian(), mode_modern("locrian"))

  expect_equal(mode_ionian(collapse = TRUE),
               mode_modern(collapse = TRUE))
  expect_equal(mode_dorian(collapse = TRUE),
               mode_modern("dorian", collapse = TRUE))
  expect_equal(mode_phrygian(collapse = TRUE),
               mode_modern("phrygian", collapse = TRUE))
  expect_equal(mode_lydian(collapse = TRUE),
               mode_modern("lydian", collapse = TRUE))
  expect_equal(mode_mixolydian("b_", collapse = TRUE),
               mode_modern("mixolydian", "b_", collapse = TRUE))
  expect_equal(mode_aeolian("c#", collapse = TRUE),
               mode_modern("aeolian", "c#", collapse = TRUE))
  expect_equal(mode_locrian(collapse = TRUE), collapse = TRUE,
               mode_modern("locrian", collapse = TRUE))

  expect_false(is_mode("c d"))
  expect_false(is_mode(c("c", "d")))
  expect_true(is_mode(mode_aeolian("b_")))
  x <- gsub("[0-9,'~]", "", transpose(mode_ionian(collapse = T), 17, key = "f"))
  expect_true(is_mode(x, ignore_octave = TRUE))
  x <- gsub("[0-9,'~]", "", transpose(mode_ionian(collapse = T), 17, key = "f"))
  expect_true(!is_mode(x))

  expect_error(mode_rotate("a"), "`notes` does not define a valid mode.")
  expect_identical(mode_rotate(mode_ionian(), 0), mode_ionian())
  expect_identical(mode_rotate(mode_ionian("c"), 1), mode_dorian("d"))

  expect_equal(mode_modern("dorian", "e", TRUE, TRUE),
               as_noteworthy("e f# g a b c# d"))
  expect_identical(mode_rotate(mode_ionian("c"), 1), mode_dorian("d"))
  expect_identical(mode_rotate(mode_ionian("c", ignore_octave = TRUE), 1),
                   mode_dorian("d", ignore_octave = TRUE))
  x <- setNames(
    data.frame(t(sapply(modes(), mode_modern, ignore_octave = TRUE))),
    as.roman(1:7))
  expect_equal(dim(x), c(7, 7))
  expect_equal(rownames(x), modes())
  expect_equal(names(x), as.character(as.roman(1:7)))
})
