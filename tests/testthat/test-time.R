context("time")

test_that("time functions return as expected", {
  a <- notate("t8x", "Start here")
  notes <- "a, b, c d e f g# a r ac'e' a c' e' c' r*3 ac'e'~ ac'e'"
  info <- paste(a, "t8x t8] 16 4.. 16- 16 2^ 2 4. 8( 4)( 4) 8*4 1 1")
  info <- as_noteinfo(info)
  x <- as_music(notes, info)

  expect_equal(n_measures(info), n_measures(x))
  expect_equal(n_beats(x, 8), 2 * n_beats(x))
  expect_equal(n_beats(x, 1), 5.375)
  expect_equal(n_beats(x, "t16"), 129)

  y <- bpm(x, "4.")
  expect_true(bpm(x, "4..") < y & y < bpm(x, "4"))
  expect_equal(bpm(x, "t8"), 1.5 * bpm(x, "8"))

  expect_equal(seconds(info, "4 = 120"), seconds(x, "2 = 60"))
  expect_equal(seconds(x, "4 = 100"), 12.9)

  d <- steps_per_measure(x)
  expect_equal(dim(d), c(6, 2))
  expect_equal(d$steps, c(7, 2, 4, 4, 1, 1))
  expect_equal(seconds_per_measure(x, tempo = "8 = 200"), 2.4)
})
