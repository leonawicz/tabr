context("helpers")

test_that("helpers return as expected.", {
  expect_equal(tie("e,,d'"), "e,,~d'~")
  expect_equal(tie("e,b,egbe'"), "e,~b,~e~g~b~e'~")
  expect_equal(rest(c(1, 8), c(1, 4)), "r1 r8 r8 r8 r8")
  expect_equal(
    as.character(phrase("c'~ c' d' e'", glue(notate(8, "First solo"), "8 8 4."), "5 5 5 5")),
    "<c'~\\5>8^\"First solo\" <c'\\5>8 <d'\\5>8 <e'\\5>4."
  )
  expect_equal(glue(8, "16-", "8^"), "8 16- 8^")
  expect_equal(dup(1, 2), "1 1")
  expect_equal(as.character(glue("r1", phrase("a", 1, 2))), "r1 <a\\2>1")
  expect_equal(as.character(dup(phrase("a", 1, 2), 2)), "<a\\2>1 <a\\2>1")
  expect_equal(hp(16, 16), "16( 16)")
  expect_equal(hp("16 16"), "16( 16)")
  expect_equal(hp("16 8 16", "8 16 8"), "16( 8) 16( 8) 16( 8)")
  expect_error(hp(8), "Even number of arguments required.")
})
