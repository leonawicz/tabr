context("data frames")

test_that("expect that as_music_df works as expected", {
  x <- "a, b, c d e f g# a r ac'e' a c' e' c' r r r a"
  d <- as_music_df(x, key = "c", scale = "major")
  expect_equal(dim(d), c(18, 11))
  expect_true(is.na(d$scale_deg[7]))
  d <- as_music_df(
    x, key = "am", scale = "harmonic_minor", si_format = "ad_abb")
  expect_equal(dim(d), c(18, 11))
  expect_equal(d$scale_deg[7], 7)

  a <- notate("8", "Start here.")
  time <- paste(a, "8*2 16 4.. 16( 16)( 2) 2 4. 8- 4 4- 8*4 1")
  d1 <- as_music_df(x, time)
  expect_equal(dim(d1), c(18, 14))
  expect_equal(d1$slur[6:9], c("on", "hold", "off", NA))

  p1 <- phrase(x, time)
  d2 <- as_music_df(p1)
  expect_identical(d1, d2)

  d3 <- as_music_df(as_music(x, time))
  expect_identical(d1, d3)

  expect_equal(dim(as_music_df("a b", 4)), c(2, 14))
  d <- as_music_df("a2 ceg", 4, chords = "character")
  expect_equal(d$semitone, c("57", "48:52:55"))
  expect_equal(d$octave, c("2", "3:3:3"))
  expect_is(d$freq, "character")
  d <- as_music_df("a2 ceg", "4->", chords = "list")
  expect_equal(d$semitone, list(57, c(48, 52, 55)))
  expect_equal(d$octave, list(2, rep(3, 3)))
  expect_is(d$freq, "list")

  expect_true("string" %in% names(as_music_df(as_music("a,8;5"))))

  expect_error(
    as_music_df("a b", "4 4 4"),
    paste("`info` must have the same number of timesteps as `notes`,",
          "or a single value to repeat, or be NULL.")
  )
})
