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
