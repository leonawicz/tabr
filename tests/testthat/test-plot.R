context("plots")

test_that("fretboard_plot runs successfully", {
  expect_is(fretboard_plot(string = 6:1, fret = c(0, 2, 2, 0, 0, 0)), "ggplot")
  expect_is(fretboard_plot(6:1, c(0, 2, 2, 0, 0, 0), c("G", "U", "I", "T", "A", "R")), "ggplot")

  string <- c(6, 6, 6, 5, 5, 5, 4, 4, 4, 4, 4, 3, 3, 3, 2, 2, 2, 1, 1, 1)
  fret <- c(2, 4, 5, 2, 4, 5, 2, 4, 6, 7, 9, 6, 7, 9, 7, 9, 10, 7, 9, 10)
  expect_is(fretboard_plot(string, fret, "notes", show_tuning = TRUE), "ggplot")
  expect_is(fretboard_plot(string, fret, "notes", show_tuning = TRUE, key = "f"), "ggplot")
  expect_is(fretboard_plot(string, fret, "notes", fret_range = c(0, 10), show_tuning = TRUE),
            "ggplot")

  tuning <- "b1 e2 a2 d3 g3 b3 e4"
  expect_is(fretboard_plot(c(7, string), c(1, fret), "notes", fret_range = c(0, 10),
                 tuning = tuning, show_tuning = TRUE), "ggplot")

  am_frets <- c(c(0, 0, 2, 2, 1, 0), c(5, 7, 7, 5, 5, 5))
  am_strings <- c(6:1, 6:1)
  mute <- c(TRUE, rep(FALSE, 12))
  idx <- c(2, 2, 1, 1, 1, 2, rep(1, 6))
  lab_col <- c("white", "black")[idx]
  pt_fill = c("firebrick1", "white")[idx]
  expect_is(fretboard_plot(am_strings, am_frets, "notes", mute,
                 label_color = lab_col, point_fill = pt_fill), "ggplot")

  f <- "0 2 2 1 0 0 0 2 2 0 0 0"
  s <- c(6:1, 6:1)
  grp <- rep(c("Open E", "Open Em"), each = 6)
  idx <- c(2, 1, 1, 1, 2, 2, 2, 1, 1, 2, 2, 2)
  lab_col <- c("white", "black")[idx]
  pt_fill = c("firebrick1", "white")[idx]
  expect_is(fretboard_plot(s, f, "notes", group = grp, fret_range = c(0, 4),
                 label_color = lab_col, point_fill = pt_fill), "ggplot")
  expect_is(fretboard_plot(string, fret, "notes", label_color = "white", point_fill = "dodgerblue",
                 fret_range = c(0, 10), show_tuning = TRUE, horizontal = TRUE), "ggplot")
  expect_is(fretboard_plot(string, fret, "notes", label_color = "white", point_fill = "dodgerblue",
                 fret_range = c(0, 10), show_tuning = TRUE, horizontal = TRUE, left_handed = TRUE),
            "ggplot")
})
