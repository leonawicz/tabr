context("music")

test_that("music functions return as expected", {
  x <- "a#4] b_ c,x d''t8( e)( g_')- a4 c,e_,g, ce_g4. a~8 a1"
  expect_false(is_music(x))
  expect_true(musical(x))
  x <- as_music(x)
  expect_is(x, "music")
  expect_true(is_music(x))

  expect_is(summary(x), "NULL")
  expect_is(print.music(x), "NULL")
  expect_equal(length(attributes(x)), 9)

  y <- music_split(x)
  expect_equal(length(y), 4)
  expect_is(music_notes(x), "noteworthy")
  expect_is(music_info(x), "noteinfo")
  expect_equal(music_tsig(x), "4/4")
  expect_equal(music_lyrics(x), NA)

  expect_true(grepl("#", as_music(x, accidentals = "sharp")))

  y <- lyrics_template(x)
  expect_equal(length(x), length(y))
  y[3:8] <- strsplit("These are some song ly- rics", " ")[[1]]
  expect_equal(as.character(y), ". . These are some song ly- rics . . .")

  x <- as_music(x, lyrics = y)
  expect_identical(y, music_lyrics(x))
})
