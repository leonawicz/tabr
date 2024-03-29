test_that("helpers return as expected.", {
  expect_error(
    pc("a1", as_music("a1")),
    "pc\\(\\) is only for phrase objects, noteworthy and simple character strings."
  )
  expect_error(
    pn(as_music("a1"), 2),
    "pn\\(\\) is only for phrase objects, noteworthy and simple character strings."
  )

  expect_equal(tie("e,,d'"), as_noteworthy("e,,~d'~"))
  expect_equal(tie("e,,d'~"), as_noteworthy("e,,~d'~"))
  expect_equal(tie("e,b,egbe'"), as_noteworthy("e,~b,~e~g~b~e'~"))
  p1 <- p("e,~b,~e~g~b~e'~ e,b,egbe'", 1)
  expect_equal(p(pc(tie("e,b,egbe'"), "e,b,egbe'"), 1), p1)
  expect_equal(p("e,b,egbe'~ e,b,egbe'", 1), p1)
  expect_equal(p("e,b,~eg~be'~ e,b,egbe'", 1), p1)

  expect_equal(rest(c(1, 8), c(1, 4)), "r1 r8 r8 r8 r8")

  expect_equal(
    as.character(phrase("c'~ c' d' e'", pc(notate(8, "First solo"), "8 8 4."),
                        "5 5 5 5")),
    "<c'~\\5>8^\"First solo\" <c'\\5>8 <d'\\5>8 <e'\\5>4."
  )

  expect_equal(pc(8, "16-", "8^"), "8 16- 8^")
  expect_equal(pn(1, 2), "1 1")
  expect_equal(pn("a", 1), pn("a", 0))
  expect_equal(pc(pn("a", 1), pn("a", 0)), pn("a", 2))
  expect_equal(as.character(pc("r1", phrase("a", 1, 2))), "r1 <a\\2>1")
  expect_equal(as.character(pn(phrase("a", 1, 2), 2)), "<a\\2>1 <a\\2>1")

  expect_equal(hp(16, 16), "16( 16)")
  expect_equal(hp("16 16"), "16( 16)")
  expect_equal(hp("16 8 16", "8 16 8"), "16( 8) 16( 8) 16( 8)")
  expect_error(hp(8), "Even number of arguments required.")

  expect_equal(as.character(tuplet("c c# d", 8)),
               "\\tuplet 3/2 4 { <c>8 <cis> <d> }")
  expect_equal(triplet("c c# d", 8), tuplet("c c# d", 8))
  expect_equal(
    as.character(tuplet("b2 c3 c# e f3 f#3", 4, "5*3 4*3", a = 6, b = 4)),
    "\\tuplet 6/4 1 { <b,\\5>4 <c\\5> <cis\\5> <e\\4> <f\\4> <fis\\4> }"
  )
  p1 <- p("c c# d c c# d", "8( 8)( 8) 8-. 8- 8", 5)
  expect_equal(as.character(tuplet(p1, 8)),
               paste("\\tuplet 3/2 4 { <c\\5>8( <cis\\5>8)( <d\\5>8)",
                     "<c\\5>8-. <cis\\5>8\\glissando <d\\5>8 }"))
  expect_equal(triplet(p1, 8), tuplet(p1, 8))
  expect_equal(
    as.character(tuplet(p1, 4, a = 6, b = 4)),
    paste("\\tuplet 6/4 1 { <c\\5>8( <cis\\5>8)( <d\\5>8)",
          "<c\\5>8-. <cis\\5>8\\glissando <d\\5>8 }")
  )
  expect_equal(as.character(triplet("c c# d", 8, string = 5)),
               "\\tuplet 3/2 4 { <c\\5>8 <cis\\5> <d\\5> }")
  expect_error(
    triplet("c c# d", 8, string = "5 4"),
    paste("`string` must have the same number of timesteps as `x`,",
          "or a single value to repeat, or be NULL.")
  )

  # lower level function
  expect_equal(.split_chord("15", strings = TRUE, abb = TRUE), "15")
  expect_equal(.split_chord("15_26", strings = TRUE, abb = TRUE), c("15", "26"))
})
