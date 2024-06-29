file <- file.path(tempdir(), "test-plot.png")

test_that("plot_fretboard runs successfully", {
  png(file)

  expect_is(plot_fretboard(string = 6:1, fret = c(0, 2, 2, 0, 0, 0)), "ggplot")
  expect_is(plot_fretboard(string = 6:1, fret = c(0, 2, 2, 0, 0, 0),
                           horizontal = TRUE), "ggplot")
  expect_is(plot_fretboard(string = 6:1, fret = c(11, 13, 13, 11, 11, 11),
                           horizontal = TRUE), "ggplot")
  expect_is(plot_fretboard(string = 6:1, fret = c(0, 2, 2, 0, 0, 0),
                           left_handed = TRUE), "ggplot")
  expect_is(plot_fretboard(string = 6:1, fret = c(0, 2, 2, 0, 0, 0), mute = 6,
                           fret_range = c(0, 2)),
            "ggplot")
  expect_is(plot_fretboard(string = 6:1, fret = c(1, 2, 2, 1, 1, 1), mute = 6,
                           fret_range = c(0, 3)),
            "ggplot")
  expect_is(
    plot_fretboard(6:1, c(0, 2, 2, 0, 0, 0), c("G", "U", "I", "T", "A", "R")),
    "ggplot")

  string <- c(6, 6, 6, 5, 5, 5, 4, 4, 4, 4, 4, 3, 3, 3, 2, 2, 2, 1, 1, 1)
  fret <- c(2, 4, 5, 2, 4, 5, 2, 4, 6, 7, 9, 6, 7, 9, 7, 9, 10, 7, 9, 10)
  expect_is(plot_fretboard(string, fret, "notes", show_tuning = TRUE), "ggplot")
  expect_is(
    plot_fretboard(string, fret, "notes", show_tuning = TRUE,
                   accidentals = "sharp"), "ggplot")
  expect_is(
    plot_fretboard(string, fret, "notes", fret_range = c(0, 10),
                   show_tuning = TRUE),
    "ggplot")

  tuning <- "b1 e2 a2 d3 g3 b3 e4"
  expect_is(
    plot_fretboard(c(7, string), c(1, fret), "notes", fret_range = c(0, 10),
                   tuning = tuning, show_tuning = TRUE),
    "ggplot")

  am_frets <- c(c(0, 0, 2, 2, 1, 0), c(5, 7, 7, 5, 5, 5))
  am_strings <- c(6:1, 6:1)
  mute <- c(TRUE, rep(FALSE, 12))
  idx <- c(2, 2, 1, 1, 1, 2, rep(1, 6))
  lab_col <- c("white", "black")[idx]
  pt_fill <- c("firebrick1", "white")[idx]
  expect_is(plot_fretboard(am_strings, am_frets, "notes", mute,
                 label_color = lab_col, point_fill = pt_fill), "ggplot")

  f <- "0 2 2 1 0 0 0 2 2 0 0 0"
  s <- c(6:1, 6:1)
  grp <- rep(c("Open E", "Open Em"), each = 6)
  idx <- c(2, 1, 1, 1, 2, 2, 2, 1, 1, 2, 2, 2)
  lab_col <- c("white", "black")[idx]
  pt_fill <- c("firebrick1", "white")[idx]
  expect_is(plot_fretboard(s, f, "notes", group = grp, fret_range = c(0, 4),
                 label_color = lab_col, point_fill = pt_fill), "ggplot")
  expect_is(
    plot_fretboard(string, fret, "notes", label_color = "white",
                   point_fill = "dodgerblue", fret_range = c(0, 10),
                   show_tuning = TRUE, horizontal = TRUE),
    "ggplot")
  expect_is(
    plot_fretboard(string, fret, "notes", label_color = "white",
                   point_fill = "dodgerblue", fret_range = c(0, 10),
                   show_tuning = TRUE, horizontal = TRUE, left_handed = TRUE),
    "ggplot")

  expect_error(plot_fretboard("5", "2 4"),
               "`string` and `fret` must have equal number of entries.")
  expect_error(plot_fretboard(string = c(1, 0), fret = 1:2),
               "String numbers must be >= 1.")
  expect_error(plot_fretboard(string = c(1, 2), fret = -1:0),
               "Fret numbers must be >= 0.")

  unlink(file, recursive = TRUE, force = TRUE)
})

test_that("plot_chord runs successfully", {
  png(file)

  idx <- c(1, 1, 2, 2, 2, 1)
  fill <- c("white", "black")[idx]
  lab_col <- c("black", "white")[idx]

  expect_is(
    plot_chord("xo221o", "notes", label_color = lab_col, point_fill = fill),
    "ggplot")
  expect_is(
    plot_chord("355433", horizontal = TRUE, show_tuning = TRUE),
    "ggplot"
  )
  expect_is(
    plot_chord("55433", horizontal = TRUE, show_tuning = TRUE),
    "ggplot"
  )
  expect_is(
    plot_chord("55433", horizontal = TRUE, show_tuning = TRUE, fret_range = c(2, 6)),
    "ggplot"
  )
  expect_is(plot_chord("0;2;3;1;"), "ggplot")
  expect_is(plot_chord("0 2 3 1"), "ggplot")
  expect_is(plot_chord("o231", fret_labels = 3), "ggplot")
  expect_is(plot_chord("10 12 13 11", show_tuning = TRUE), "ggplot")
  expect_is(
    plot_chord("o x 10 12 13 11", fret_range = c(9, 14), fret_labels = c(9, 12)),
    "ggplot"
  )

  expect_error(plot_chord(c("xo221o", "xo221o")),
               "Length of chord must be one.")
  expect_error(
    plot_chord("022300", tuning = "bass"),
    "Cannot have more fret values than number of instrument strings."
  )
  expect_error(
    plot_chord("o x 10 12 13 11", fret_range = c(10, 14), fret_labels = c(9, 12)),
    "Explicit fret range will cut off notes."
  )
  expect_error(
    plot_chord("o o 10 12 13 11", fret_range = c(8, 14), fret_labels = c(9, 12)),
    "Explicit fret range must be greater than implicit fret number of explicit open strings."
  )
  expect_error(
    plot_chord("o o 10 12 13 11", fret_range = 9:14),
    "Explicit fret range must be a vector of two numbers."
  )

  unlink(file, recursive = TRUE, force = TRUE)
})
