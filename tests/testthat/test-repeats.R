test_that("repeat functions return as expected", {
  x <- phrase("c ec'g' ec'g'", "4 4 2", "5 432 432")
  e1 <- phrase("a", 1, 5)
  e2 <- phrase("b", 1, 5)

  expect_equal(as.character(rp(x)),
    "\\repeat unfold 2 { <c\\5>4 <e\\4 c'\\3 g'\\2>4 <e\\4 c'\\3 g'\\2>2 }\n")
  expect_equal(as.character(rp(x, 3)),
    "\\repeat unfold 4 { <c\\5>4 <e\\4 c'\\3 g'\\2>4 <e\\4 c'\\3 g'\\2>2 }\n")
  expect_equal(as.character(pct(x)),
    "\\repeat percent 2 { <c\\5>4 <e\\4 c'\\3 g'\\2>4 <e\\4 c'\\3 g'\\2>2 }\n")
  expect_equal(as.character(pct(x, 9, TRUE, 5)),
    paste0(
      "\\set countPercentRepeats = ##t\n",
      "\\set repeatCountVisibility = #(every-nth-repeat-count-visible 5)\n",
      "\\repeat percent 10 ",
        "{ <c\\5>4 <e\\4 c'\\3 g'\\2>4 <e\\4 c'\\3 g'\\2>2 }\n",
      "\\set countPercentRepeats = ##f\n",
      "\\set repeatCountVisibility = #(every-nth-repeat-count-visible 1)\n"))
  expect_equal(as.character(pct(x, 9, TRUE, 5, FALSE)),
    paste0(
      "\\set countPercentRepeats = ##t\n",
      "\\set repeatCountVisibility = #(every-nth-repeat-count-visible 5)\n",
      "\\repeat percent 10 ",
        "{ <c\\5>4 <e\\4 c'\\3 g'\\2>4 <e\\4 c'\\3 g'\\2>2 }\n"))

  expect_equal(as.character(volta(x)),
    "\\repeat volta 2 { <c\\5>4 <e\\4 c'\\3 g'\\2>4 <e\\4 c'\\3 g'\\2>2 | }\n")
  expect_equal(as.character(volta(x, 1, list(e1, e2))),
    paste0(
    "\\repeat volta 2 { <c\\5>4 <e\\4 c'\\3 g'\\2>4 <e\\4 c'\\3 g'\\2>2 | }\n",
    "\\alternative {\n  { <a\\5>1 | }\n  { <b\\5>1 | }\n}"))
  expect_equal(as.character(volta(x, 4, list(e1, e2))),
    paste0(
    "\\repeat volta 5 { <c\\5>4 <e\\4 c'\\3 g'\\2>4 <e\\4 c'\\3 g'\\2>2 | }\n",
    "\\alternative {\n  { <a\\5>1 | }\n  { <b\\5>1 | }\n}"))
  expect_equal(as.character(volta(x, 4)),
               paste0("\\repeat volta 5 { <c\\5>4",
                      "^\"Play 5 times.\"",
                      " <e\\4 c'\\3 g'\\2>4 <e\\4 c'\\3 g'\\2>2 | }\n"))

  expect_error(volta(x, endings = 1:2),
               "`endings` must be a list of phrases or a single phrase.")
  expect_is(volta(x, 4, e1), "phrase")
})
