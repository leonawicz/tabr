context("snippet")

test_that("functions run as expected", {
  x <- "a,4;5*5 b,4- c4 cgc'e'~4 cgc'e'1 e'4;2 c';3 g;4 c;5 ce'1;51"
  x <- as_music(x)

  y <- "a,,4;3*5 b,,4- c,4 c,g,c~4 c,g,c1 c4;1 g,;2 c,;3 g,;2 c,c1;31"
  y <- as_music(y)

  outfile <- file.path(tempdir(), "out.pdf")
  expect_is(render_music(x, outfile), "NULL")

  outfile <- file.path(tempdir(), "out.png")
  expect_is(render_music(x, outfile, "treble_8", no_tab = FALSE), "NULL")

  expect_is(render_music_tc(x, outfile), "NULL")
  expect_is(render_music_bc(x, outfile), "NULL")

  expect_is(render_music_tab(x, outfile), "NULL")
  expect_is(render_music_guitar(x, outfile), "NULL")
  expect_is(render_music_bass(y, outfile), "NULL")

  expect_error(render_music_bass(x, outfile),
               "String number exceeds number of strings from `tuning`.")

})
