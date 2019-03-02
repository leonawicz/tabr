context("transpose")

library(dplyr)

test_that("transpose returns as expected.", {
  expect_equal(transpose("b_,", -1, "am"), as_noteworthy("a,"))
  expect_equal(transpose("b_2", 1, "c"), as_noteworthy("b2"))

  x <- list("a_3 b_4 c5", "a#3 b4 c#5", "a3 b4 c5")
  expect_equal(transpose(x[[1]], 0) %>% as.character(), x[[1]])
  expect_equal(tp("c#3 a_' d#,", 0, "f") %>% as.character(), "d_ a_' e_,")
  expect_equal(tp("d_4 a2 e_2", 0, "g") %>% as.character(), "c#4 a2 d#2")

  expect_equal(tp(x[[1]], -1) %>% as.character(), "g a4 b4")
  expect_equal(tp(x[[1]], 1) %>% as.character(), "a b4 c#5")
  expect_equal(tp(x[[2]], 11) %>% as.character(), "a4 a#5 c6")
  expect_equal(tp(x[[2]], 12) %>% as.character(), "a#4 b5 c#6")
  expect_equal(tp(x[[2]], 13) %>% as.character(), "b4 c6 d6")
  expect_equal(tp(x[[3]], 2, key = "f") %>% as.character(), "b d_5 d5")
  expect_equal(tp(x[[3]], 2, key = "g") %>% as.character(), "b c#5 d5")
  expect_equal(tp(x[[3]], 2, key = "g", style = "strip") %>% as.character(), "b c# d")

  expect_equal(tp("a b' c''", 2, key = "f") %>% as.character(), "b d_'' d''")
  expect_equal(tp("a b' c''", 2, key = "flat") %>% as.character(), "b d_'' d''")
  expect_equal(tp("a, b3 c'", 2, key = "g") %>% as.character(), "b, c#' d'")
  expect_equal(tp("a2 b c4", 2, key = "sharp", style = "tick") %>% as.character(), "b, c#' d'")

  expect_equal(tp("a b' c''", 2, key = "f", style = "integer") %>% as.character(), "b d_5 d5")
  expect_equal(tp("a b' c''", 2, key = "flat", style = "integer") %>% as.character(), "b d_5 d5")
  expect_equal(tp("a, b c'", 2, key = "g", style = "integer") %>% as.character(), "b2 c#4 d4")
  expect_equal(tp("a, b c'", 2, key = "sharp", style = "integer") %>% as.character(), "b2 c#4 d4")

  x <- as_noteworthy("r b2 c#4 s d4")
  y <- as_noteworthy("r b, c#' s d'")
  expect_equal(tp("r a, b s c'", 2, key = "g", "integer"), x)
  expect_equal(tp("r a, b s c4", 2, key = "sharp"), y)
  expect_equal(tp("r a, b s c'", 2, key = "g", "integer"), x)
  expect_equal(tp("r a, b s c4", 2, key = "sharp"), y)
  expect_equal(tp("s a,~ a, b r c'~", 2, key = "g", "integer"), as_noteworthy("s b2~ b2 c#4 r d4~"))

  x <- as_noteworthy("a' a# b_2 b~ b ceg c'e'g' ce3g c4e4g4 c,e2g, d_,e_ e_'")
  expect_equal(transpose(x, 3, "flat"),
               as_noteworthy("c'' d_' d_ d'~ d' e_gb_ e_'g'b_' e_gb_ e_'g'b_' e_,g,b_, e,g_ g_'"))
  expect_equal(transpose(x, 3, style = "int"),
               as_noteworthy("c5 c#4 c# d4~ d4 d#ga# d#4g4a#4 d#ga# d#4g4a#4 d#2g2a#2 e2f# f#4"))

  expect_error(tp("a.", 1), "Invalid notes or chords found.")
  expect_error(tp("a.. c", 1), "Invalid notes or chords found.")
})
