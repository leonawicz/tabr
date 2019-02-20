context("scales")

test_that("scales return as expected", {
  expect_equal(scale_diatonic("c", collapse = TRUE), "c d e f g a b")
  expect_equal(scale_diatonic("am", collapse = TRUE), "a b c d e f g")
  expect_equal(length(scale_chromatic("c")), 12)

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
