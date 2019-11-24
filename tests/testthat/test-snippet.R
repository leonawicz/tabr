context("snippet")

x <- "a,4;5*5 b,4- c4 cgc'e'~4 cgc'e'1 e'4;2 c';3 g;4 c;5 ce'1;51"
x <- as_music(x)

y <- "a,,4;3*5 b,,4- c,4 c,g,c~4 c,g,c1 c4;1 g,;2 c,;3 g,;2 c,c1;31"
y <- as_music(y)

outfile1 <- file.path(tempdir(), "out.pdf")
outfile2 <- file.path(tempdir(), "out.png")

test_that("render functions run as expected", {
  expect_is(render_music(x, outfile1), "NULL")

  expect_is(render_music(x, outfile2, "treble_8", tab = TRUE), "NULL")

  expect_is(render_music_tc(x, outfile2), "NULL")
  expect_is(render_music_bc(x, outfile2), "NULL")

  expect_is(render_music_tab(x, outfile2), "NULL")
  expect_is(render_music_guitar(x, outfile2), "NULL")
  expect_is(render_music_bass(y, outfile2), "NULL")

  expect_error(render_music_bass(x, outfile),
               "String number exceeds number of strings from `tuning`.")
  unlink(c(outfile1, outfile2), recursive = TRUE, force = TRUE)
})

test_that("plot wrappers run as expected", {
  skip_on_appveyor()
  skip_on_cran()
  if(require(png)){
    png(outfile2)
    expect_is(plot_music(x), "NULL")

    expect_is(plot_music(x, "treble_8", tab = TRUE), "NULL")

    expect_is(plot_music_tc(x), "NULL")
    expect_is(plot_music_bc(x), "NULL")

    expect_is(plot_music_tab(x), "NULL")
    expect_is(plot_music_guitar(x), "NULL")
    expect_is(plot_music_bass(y), "NULL")

    expect_error(render_music_bass(x),
                 "String number exceeds number of strings from `tuning`.")
    dev.off()
    unlink(outfile2, recursive = TRUE, force = TRUE)
  }
})
