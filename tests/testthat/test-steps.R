context("steps")

p1 <- p("r a2 c3 f3 d3 a3 f3", "4 8 8 8 8 8 8", "x 5 5 4 4 3 4")
track1 <- track(p1)
song <- score(track1)

test_that("Simple phrase to score example works", {
  expect_is(p1, "phrase")
  expect_is(p1, "character")
  expect_is(track1, "track")
  expect_is(song, "score")
  expect_is(trackbind(track1), "track")
  expect_is(trackbind(track1, track1), "track")
})

p1 <- p("r a2 c3 f3 d3 a3 f3", "4 8 8 8 8 8 8")
x <- list(
  track(p1),
  track(p1, tuning = "DADGAD"),
  track(p1, music_staff = "treble"),
  track(p1, voice = 2),
  track(p1, music_staff = NA),
  track(p1, ms_transpose = -2),
  track(p1, ms_transpose = 1, ms_key = "sharp")
)

test_that("track arguments accepted", {
  cl <- "track"
  purrr::walk(x, ~expect_is(.x, cl))
  x2 <- do.call(trackbind, x)
  x3 <- trackbind(x[[3]], x[[2]], x[[4]], x[[5]], tabstaff = c(1, 2, 2, 3))
  expect_is(x2, cl)
  expect_equal(nrow(x2), length(x))
  expect_is(score(x2), "score")
  expect_equal(nrow(score(x2)), nrow(x2))
  expect_is(x3, cl)
  expect_equal(nrow(x3), 4)
  expect_is(score(x3), "score")
  expect_equal(nrow(score(x3)), nrow(x3))
  expect_equal(ncol(x3), ncol(x[[1]]) + 1)
})
