context("sf")

s <- c("5 4 3 2 1", "654321 6s 12 1 21", "6s*2 1*4")
i <- c("8*4 1", "4. 8 8 8 4", "4- 4 8] 8( 8)( 8)")
f <- c("1 3 3 3 1", "133211 355333 11 (13) (13)(13)", "111343 333565 1 1 0 1")
n <- c("b_, f b_ d' f'", "a#, f a# d' f'", "b_2 f b_ c4 e_4",
       "f2cfac4f4 g,dgb_d'g' f'c' f'' c''f''",
       "d#,a#,d#a#d#'g' f,cfc'f'a' f' f' e' f'",
       "e_,b_,e_b_e_'g' f,cfc'f'a' f' f' e' f'")

sf_list <- list(
  sf_phrase(s[1], f[1], i[1], key = "b_"),
  sf_phrase(s[1], f[1], i[1], key = "sharp"),
  sf_phrase(s[1], f[1], i[1], tuning = "DADGAD", key = "flat", bar = TRUE),
  sf_phrase(s[2], f[2], i[2], key = "f"),
  sfp(s[3], f[3], i[3], tuning = "dropD", key = "d"),
  sfp(s[3], f[3], i[3], tuning = "dropD", key = "e_")
)
p_list <- list(
  p(n[1], i[1], s[1]),
  p(n[2], i[1], s[1]),
  p(n[3], i[1], s[1], bar = TRUE),
  p(n[4], i[2], s[2]),
  p(n[5], i[3], s[3]),
  p(n[6], i[3], s[3])
)

test_that("sf_phrase and phrase calls are equivalanet", {
  err <- c("String/fret mismatch.", "Rest mismatch.", "Tied note mismatch.")
  expect_is(sfp("6 6", "1", 1), "phrase")
  expect_error(sfp("66", "1", 1), err[1])
  expect_error(sfp("s", "r", 1), err[2])
  expect_error(sfp("6 r", "1", 1), err[2])
  expect_error(sfp("6 s 6 r", "1 s 1 s", 1), err[2])
  expect_error(sfp("6~ 6", "1", 1), err[3])
  expect_error(sfp("6~ 6", "1 1", 1), err[3])
  expect_error(sfp("r 6~ 6", "s 1 1", 1), err[2])
  expect_error(sfp("r 6~ 6 s", "r 1 1 s", 1), err[3])

  purrr::walk2(sf_list, p_list, ~expect_identical(.x, .y))

  expect_identical(sfp("r", "r", 1), p("r", 1))
  expect_identical(sfp("s", "s", 1), p("s", 1))
  expect_identical(sfp("r r", "r r", 1), p("r r", 1))
  expect_identical(sfp("s s", "s s", 1), p("s s", 1))

  info <- "2 1 1 2"
  expect_identical(sfp("s 6~ 6 r", "s 1~ 1 r", info), p("s f,~ f, r", info, "x 6 6 x"))
  expect_identical(sfp("s 6s~ 6s r", "s 133211~ 133211 r", info), p("s f,cfac'f'~ f,cfac'f' r", info, "x 6s 6s x"))

  strings <- "4~ 4 r 5s s 5"
  frets <- "1~ 1 r 02220 s 2"
  info <- "1 4. 8 2 16 2..."
  expect_identical(sfp(strings, frets, info, key = "sharp"),
                   p("d#~ d# r a,eac#'e' s b,", info, "4 4 x 5s x 5"))
  x <- sf_phrase(strings, frets, info, key = "sharp", to_notes = TRUE)
  expect_identical(x, sfp(strings, frets, info, key = "sharp", to_notes = TRUE))
  expect_identical(x, sf_note(strings, frets, info, key = "sharp"))
  expect_identical(x, sfn(strings, frets, info, key = "sharp"))
})
