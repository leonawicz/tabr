context("transpose")

test_that("transpose returns as expected.", {
  x <- list("a_3 b_4 c5", "a#3 b4 c#5", "a3 b4 c5")
  expect_equal(transpose(x[[1]], 0), x[[1]])
  expect_equal(tp(x[[1]], -1), "g3 a4 b4")
  expect_equal(tp(x[[1]], 1), "a3 b4 c#5")
  expect_equal(tp(x[[2]], 11), "a4 a#5 c6")
  expect_equal(tp(x[[2]], 12), "a#4 b5 c#6")
  expect_equal(tp(x[[2]], 13), "b4 c6 d6")
  expect_equal(tp(x[[3]], 2, key = "f"), "b3 d_5 d5")
  expect_equal(tp(x[[3]], 2, key = "g"), "b3 c#5 d5")
  expect_equal(tp("a b' c''", 2, key = "f"), "b3 d_5 d5")
  expect_equal(tp("a, b c'", 2, key = "g"), "b2 c#4 d4")

  expect_equal(tp("r a, b r c'", 2, key = "g"), "r b2 c#4 r d4")
  expect_equal(tp("r a,~ a, b r c'~", 2, key = "g"), "r b2~ b2 c#4 r d4~")

  expect_error(tp("a.", 1), "`notes` is not a valid string of notes.")
})
