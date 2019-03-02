context("modes")

test_that("mode helpers return as expected", {
  expect_equal(mode_ionian(), mode_modern())
  expect_equal(mode_dorian(), mode_modern("dorian"))
  expect_equal(mode_phrygian(), mode_modern("phrygian"))
  expect_equal(mode_lydian(), mode_modern("lydian"))
  expect_equal(mode_mixolydian("b_"), mode_modern("mixolydian", "b_"))
  expect_equal(mode_aeolian("c#"), mode_modern("aeolian", "c#"))
  expect_equal(mode_locrian(), mode_modern("locrian"))

  expect_true(is_mode(mode_aeolian("b_")))
  expect_true(is_mode(transpose(mode_ionian(collapse = T), 17, "f", style = "strip"), ignore_octave = TRUE))
  expect_true(!is_mode(transpose(mode_ionian(collapse = T), 17, style = "strip")))

  expect_error(mode_rotate("a"), "`notes` does not define a valid mode.")
  expect_identical(mode_rotate(mode_ionian(), 0), mode_ionian())
  expect_identical(mode_rotate(mode_ionian("c"), 1), mode_dorian("d"))

  expect_equal(mode_modern("dorian", "e", TRUE, TRUE), as_noteworthy("e f# g a b c# d"))
  expect_identical(mode_rotate(mode_ionian("c"), 1), mode_dorian("d"))
  expect_identical(mode_rotate(mode_ionian("c", ignore_octave = TRUE), 1),
                   mode_dorian("d", ignore_octave = TRUE))
  x <- setNames(data.frame(t(sapply(modes(), mode_modern, ignore_octave = TRUE))), as.roman(1:7))
  expect_equal(dim(x), c(7, 7))
  expect_equal(rownames(x), modes())
  expect_equal(names(x), as.character(as.roman(1:7)))
})
