context("phrase")

eflats <- c("e_,, e_, e_ e_' e_''", "e_1 e_2 e_3 e_4 e_5")
efchord <- gsub(" ", "", eflats)
x <- purrr::map(c(eflats, efchord), ~phrase(.x, 4))
y <- c(rep("<es,,>4 <es,>4 <es>4 <es'>4 <es''>4", 2), rep("<es,, es, es es' es''>4", 2))

test_that("phrase returns as expected", {
  purrr::walk(x, ~expect_is(.x, "phrase"))
  purrr::walk2(as.character(x), y, ~expect_equal(.x, .y))

  expect_equal(as.character(p("de_' aa_,", 1)), "<d es'>1 <a as,>1")
  expect_equal(as.character(p("ee_' a_a#,", "2")), "<e es'>2 <as ais,>2")
  expect_equal(as.character(p("d,, e_e_ a_' b_'d", "1 2 4.. 8.")), "<d,,>1 <es es>2 <as'>4.. <bes' d>8.")
  expect_equal(as.character(p("d,, e_e_ a_' b_'d", "1 2 4.. 8.", abb = FALSE)),
               "<d,,>1 <ees ees>2 <aes'>4.. <bes' d>8.")

  y <- "<d,,\\1>1 <es\\2 es\\1>2 <as'\\3>4.. <bes'\\4 d\\3>8."
  expect_equal(as.character(p("d,, e_e_ a_' b_'d", "1 2 4.. 8.", "1 21 3 43")), y)
  expect_equal(as.character(p("d1 e_3e_ a_' b_'d", "1 2 4.. 8.", "1 21 3 43")), y)

  n <- "d,,e_e_3 a_4b_4d"
  y <- "<d,,\\3 es\\2 es\\1>2 <as'\\3 bes'\\2 d\\1>2"
  expect_equal(as.character(p(n, "2 2", "321 321")), y)
  expect_equal(as.character(p(n, "2*2", "321*2")), y)
  expect_equal(as.character(p(n, 2, "321 3s")), y)

  x1 <- phrase("c b, c", "4. 8( 8)", "5 5 5") # direction implies hammer on
  x2 <- phrase("b2 c d", "4( 4)- 2", "5 5 5") # hammer and slide
  expect_equal(as.character(x1), "<c\\5>4. <b,\\5>8( <c\\5>8)")
  expect_equal(as.character(x2), "<b,\\5>4( <c\\5>4)\\glissando <d\\5>2")
})
