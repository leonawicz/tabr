context("strings")

test_that("string fold and unfold returns as expected", {
  time <- "8*3 16 4.. 16 16 2 2 4. 8 4 4 8*4 1"
  x <- string_unfold(time)
  expect_equal(x, "8 8 8 16 4.. 16 16 2 2 4. 8 4 4 8 8 8 8 1")
  expect_equal(string_fold(x), time)

  notes <- "a, b, c d e f g# a r ac'e' a c' e' c' r r r a"
  x <- string_fold(notes)
  expect_equal(string_unfold(x), notes)

  expect_equal(string_unfold(c("a*4", "b")), c(rep("a", 4), "b"))
})
