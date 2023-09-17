test_that("substitutions work as intended", {
  x <- "aes as es ees c' c4 d, d2 a3 a ees,desdes'"
  y <- "a_ a_ e_ e_ c' c4 d, d2 a3 a e_,d_d_'"
  expect_equal(.noterev(x), y)
})
