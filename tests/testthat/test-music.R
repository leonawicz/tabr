context("music")

test_that("music functions return as expected", {
  x <- "a#4-. b_ c,x d''t8( e)( g_')- a4 c,e_,g, ce_g4. a~8 a1"
  expect_false(is_music(x))
  expect_true(musical(x))
  x <- as_music(x)
  expect_is(x, "music")
  expect_true(is_music(x))
  expect_true(musical(x))

  expect_is(summary(x), "NULL")
  expect_is(print.music(x), "NULL")
  expect_equal(length(attributes(x)), 11)

  y <- music_split(x)
  expect_equal(length(y), 6)
  expect_is(music_notes(x), "noteworthy")
  expect_is(music_info(x), "noteinfo")
  expect_equal(music_key(x), "c")
  expect_equal(music_time(x), "4/4")
  expect_equal(music_tempo(x), "2 = 60")
  expect_equal(music_lyrics(x), NA)
  expect_equal(music_strings(x), NULL)

  expect(n_steps(x), 11)
  expect(n_steps(as.character(x)), 11)
  expect_true(is_space_time(x))
  expect_false(is_vector_time(x))
  expect_equal(as_space_time(x), x)
  expect_equal(as_vector_time(x), as_music(x, format = "vector"))
  expect_equal(as_space_time(as.character(x)), x)
  expect_equal(as_vector_time(as.character(x)),
               as_music(x, format = "vector"))
  expect_true(is_space_time("a,4--"))
  expect_true(is_vector_time(c("a4--", "b4-+")))
  expect_equal(as.character(as_music(c("a", "b"), 2)), c("a2", "b2"))
  expect_equal(time_format("a,4"), "space-delimited time")
  expect_equal(time_format(c("a,4", "b,4")), "vectorized time")

  expect_error(as_music("a b4"), "First timestep must have a duration value.")

  z <- music_split("a4")
  expect_identical(z, music_split(as_music("a4")))
  expect_error(music_split("a5"), "Invalid note info found.")

  expect_true(grepl("#", as_music(x, accidentals = "sharp")))

  expect_error(as_music(x, lyrics = "a"),
               "`lyrics` must be a `lyrics`-class object or NA.")
  y <- lyrics_template(x)
  expect_equal(length(x), length(y))
  y[3:8] <- strsplit("These are some song ly- rics", " ")[[1]]
  expect_equal(as.character(y), ". . These are some song ly- rics . . .")

  x <- as_music(x, lyrics = y)
  expect_identical(y, music_lyrics(x))
  x <- as_music(x, lyrics = as_vector_time(y))
  expect_identical(y, music_lyrics(x))

  x <- as_music(as_vector_time(x), lyrics = y)
  expect_identical(as_vector_time(y), music_lyrics(x))

  expect_true(all(is.na(info_annotation(x))))
  expect_equal(info_articulation(x), c("-.", rep(NA, 10)))

  expect_identical(c(as_space_time(x), x), as_space_time(c(x, x)))

  y[1:11] <- letters[1:11]

  x <- as_music(x, lyrics = y, labels = "some text", at = 2)
  x[3] <- "c,4[staccato]"

  expect_equal(info_annotation(x)[2], "some text")
  expect_is(print.music(x), "NULL")
  expect_is(summary(x), "NULL")

  x <- "a,4;5*5 b,4- c4 cgc'e'~4 cgc'e'1 e'4;2 c';3 g;4 c;5 ce'1;51"
  s <- as.character(c(rep(5, 7), 5432, 5432, 2, 3, 4, 5, 51))
  x <- as_music(x)
  expect_equal(music_strings(x), s)
  y <- music_split(x)
  expect_equal(length(y), 7)
  expect_equal(y[[3]], y$string)
  expect_equal(y$string, s)
  expect_equal(music_strings(x), s)
  expect_is(summary(x), "NULL")

  expect_error(as_music("ab4;0"), "Invalid string number < 1.")
  expect_error(
    as_music("ab4;5 a;54"),
    "Number of strings and notes must match at each non-rest timestep.")
  expect_error(as_music("a", "4 4"),
               "`notes` and `info` have unequal number of timesteps.")
  expect_error(music_split("a4 b4;^\"some_text\" c'8[hello world]"),
               "Invalid notes or chords found.")
  expect_error(music_split("a4 b4;^\"some_text\" c'8[hello_world]"),
               "Invalid notes or note info found.")
})
