context("phrase")

eflats <- c("e_,, e_, e_ e_' e_''", "e_1 e_2 e_3 e_4 e_5")
efchord <- gsub(" ", "", eflats)
x <- purrr::map(c(eflats, efchord), ~phrase(.x, 4))
y <- c(rep("<es,,>4 <es,>4 <es>4 <es'>4 <es''>4", 2),
       rep("<es,, es, es es' es''>4", 2))

test_that("phrase returns as expected", {
  purrr::walk(x, ~expect_is(.x, "phrase"))
  purrr::walk2(as.character(x), y, ~expect_equal(.x, .y))

  expect_is(print(x[[1]]), "NULL")
  expect_identical(phrase(c("b_", "c"), 1), phrase("b_ c", 1))
  expect_identical(as.character(p("c d", "8x 8^", 1)),
                   "\\deadNote <c\\1>8 <d\\1>8\\bendAfter #+6")

  expect_equal(as.character(p("de_' aa_,", 1)), "<d es'>1 <a as,>1")
  expect_equal(as.character(p("ee_' a_a#,", "2")), "<e es'>2 <as bes,>2")
  expect_equal(as.character(p("d,, e_e_ a_' b_'d", "1 2 4.. 8.")),
               "<d,,>1 <es es>2 <as'>4.. <bes' d>8.")

  y <- "<d,,\\1>1 <es\\2 es\\1>2 <as'\\3>4.. <bes'\\4 d\\3>8."
  expect_equal(
    as.character(p("d,, e_e_ a_' b_'d", "1 2 4.. 8.", "1 21 3 43")), y)
  expect_equal(
    as.character(p("d1 e_3e_ a_' b_'d", "1 2 4.. 8.", "1 21 3 43")), y)

  n <- "d,,e_e_3 a_4b_4d"
  y <- "<d,,\\3 es\\2 es\\1>2 <as'\\3 bes'\\2 d\\1>2"
  expect_equal(as.character(p(n, "2 2", "3 3")), y)
  expect_equal(as.character(p(n, "2*2", "3*2")), y)
  expect_equal(as.character(p(n, 2, "3 321")), y)

  x1 <- phrase("c b, c", "4. 8( 8)", "5 5 5") # direction implies hammer on
  x2 <- phrase("b2 c d", "4( 4)- 2", "5 5 5") # hammer and slide
  expect_equal(as.character(x1), "<c\\5>4. <b,\\5>8( <c\\5>8)")
  expect_equal(as.character(x2), "<b,\\5>4( <c\\5>4)\\glissando <d\\5>2")

  expect_error(p(1:2, 1, 1), "Invalid notes or chords found.")
  expect_error(
    p("a", 1:2, 1),
    paste("`info` must have the same number of timesteps as `notes`",
           "or a single value to repeat.")
  )
  expect_error(
    p("a", 1, 1:2),
    paste("`string` must have the same number of timesteps as `notes`,",
          "or a single value to repeat, or be NULL.")
  )

  expect_equal(.notesub("ees", simplify = TRUE), "es")
  expect_identical(p("a", 1), p("a", 1, NA))
  expect_identical(
    p("c d e f g a b c'", 8, bar = NULL),
    p("c d e f g a b c'", 8, bar = FALSE)
  )
  expect_identical(
    p("c d e f g a b c'", 8),
    gsub(" \\|$", "", p("c d e f g a b c'", 8, bar = TRUE))
  )
  expect_identical(
    p("c d e f g a b c'", 8),
    gsub(" \\\\bar \"\\|\\.\"$", "", p("c d e f g a b c'", 8, bar = "|."))
  )
})

p1 <- phrase("c ec'g' ec'g'", "4 4 2") # no explicit strings (not recommended)
p2 <- phrase("c ec4g4 ec4g4", "4 4 2") # same as above
p3 <- phrase("c b, c", "4. 8( 8)", "5 5 5") # direction implies hammer on
p4 <- phrase("b2 c d", "4( 4)- 2", "5 5 5") # hammer and slide
p5 <- phrase("c ec'g'~ ec'g'", 1, "5 432 432") # tied chord
p6 <- phrase("s r c ec'g'~ ec'g' r", 1, "5 x x 432 432 x") # add rests
x <- list(p1, p2, p3, p4, p5, p6)

y <- lapply(as.character(x), as_phrase)

test_that("phrasey returns as expected", {
  expect_true(all(sapply(x, phrasey)))
  expect_true(all(sapply(as.character(x), phrasey)))

  expect_identical(x, y)

  expect_false(phrasey(1))
  expect_false(phrasey("x"))
  expect_false(phrasey("<a"))
  expect_true(phrasey("r1 s2"))
  expect_true(
    phrasey(p("r s", pc(notate(4, "Note 1"), notate("4..", "Note 2"))))
  )
  expect_true(
    phrasey(p("s", pc(notate("4..", "Note 2"))))
  )
  expect_true(
    phrasey(p("a b r", pc(4, notate(4, "Note 1"), notate("4..", "Note 2"))))
  )
})

test_that("as_phrase returns as expected", {
  expect_identical(x, lapply(x, as_phrase))
  expect_identical(x, lapply(y, as_phrase))

  expect_identical(x, y)

  expect_error(as_phrase(1), paste("Cannot coerce numeric to phrase."))
  expect_error(as_phrase("r"), paste("`x` is not phrasey."))
})

test_that("notable returns as expected", {
  expect_true(all(sapply(x, notable)))
  expect_true(notable(p("a b c", 1)))
  expect_false(notable("a b x"))
})

test_that("notification works as expected", {
  d <- do.call(rbind, lapply(x, notify))
  expect_is(d, "tbl_df")
  expect_equal(dim(d), c(21, 3))
  expect_true(all(is.na(d$string[1:6])))

  expect_equal(
    notify(p("a b", pc(notate("4", "Start here."), "4x")))$info,
    c("4;^\"Start_here.\"", "4x")
  )
  expect_equal(phrase_info(p("a b", pc(notate("4", "Z"), "4")), FALSE, FALSE),
               as_noteinfo(c("4", "4")))
  x2 <- lapply(x, function(x){
    p(phrase_notes(x), phrase_info(x), phrase_strings(x))
  })
  identical(x, x2)
  expect_error(notify("a b>"), "`phrase` is not phrasey.")
  expect_error(notify(p(as_music("at8"))),
               "Cannot notify phrases containing tuplets.")
})

test_that("Other functions work on phrases as expected", {
  expect_equal(info_articulation(phrase("a b", "4-+ 4.[staccato]")),
               c("-+", "staccato"))
  expect_error(simplify_phrase(1), "Not a phrase.")
})

notes1 = "a ^1 b ^2 c" ; info1= "4 1 4 1 4 " ; string1 = "1 x 2 x 3"
notes2 = "a ^1 b ^2 c" ; info2= "4" ; string2 = "1"
# notes3 = "a ^  b ^  c" ; info3= "4" ; string3 = "1"
notes4 = "a ^ b ^ c"   ; info4= "4" ; string4 = "1"
notes5 = "a b"   ; info5= "4" ; string5 = "1"
notes6 = "a ^1 b ^2 c5" ; info6= "4 1 4 1 4" ; string6 = "1 x 2 x 6"

test_that("phrase works for hook ^", {
  expect_equal(class(p(notes1,info1,string1)),
              c("phrase", "character") )
  expect_equal(as.character(p(notes1,info1,string1)),
              "<a\\1>4 ^1 <b\\2>4 ^2 <c\\3>4" )
  expect_equal(as.character(p(notes2,info2,string2)),
               "<a\\1>4 ^1 <b\\1>4 ^2 <c\\1>4" )
  # expect_error(as.character(p(notes3,info3,string3)),
  #              "Invalid notes or chords found." )
  expect_equal(as.character(p(notes4,info4,string4)),
               "<a\\1>4 ^ <b\\1>4 ^ <c\\1>4" )
  expect_equal(as.character(p(notes5,info5,string5)),
               "<a\\1>4 <b\\1>4" )
  expect_equal(as.character(p(notes6,info6,string6)),
               "<a\\1>4 ^1 <b\\2>4 ^2 <c''\\6>4" )
})

test_that("phrasey works for hook ^", {
  expect_true(phrasey("<a\\1>3 ^1 <b\\2>4"))
  expect_true(phrasey("<a\\1>3 ^ <b\\2>4"))
})


