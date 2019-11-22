context("lyrics")

test_that("lyrics functions return as expected", {
  x <- "These are the ly- rics . . . to this song"
  expect_false(is_lyrics(x))
  expect_true(lyrical(x))
  y <- as_lyrics(x)
  expect_is(y, "lyrics")
  expect_true(is_lyrics(y))
  expect_true(lyrical(y))
  expect_equal(as_lyrics(y), y)

  x2 <- c("These", "are", "the", "ly-", "rics",
         "", ".", NA, "to", "this", "song") #
  y2 <- as_lyrics(x2)
  expect_equal(time_format(y2), "vectorized time")
  expect_identical(y, as_space_time(y2))

  notes <- as_noteworthy("c d e d c r*3 e g c'")
  z <- lyrics_template(notes)
  expect_is(z, "lyrics")
  expect_equal(length(notes), length(z))
  expect_true(all(as.character(as_vector_time(z)) == "."))

  z[1:5] <- strsplit("These are the ly- rics", " ")[[1]]
  z[9:11] <- c("to", "this", "song")
  expect_identical(y, z)

  expect_is(summary(z), "NULL")
  expect_is(print.lyrics(z), "NULL")
  expect_equal(length(attributes(z)), 5)

  expect_equal(lyrics_template(5), as_lyrics(rep(".", 5)))
  expect_error(
    lyrics_template("a"),
    "`x` must be an integer, or `noteworthy`, `noteinfo` or `music`.")

  expect_error(as_lyrics(1), "Lyrics do not inherit from character.")
})
