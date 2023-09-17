library(dplyr)

test_that("note helpers return as expected", {
  notes <- "bd'f#' a c'e'g' b ba c'g' gd'g'd''"
  expect_identical(note_sort(notes),
                   as_noteworthy("gd'g'd'' a ba b bd'f#' c'e'g' c'g'"))
  expect_equal(note_sort(notes, decreasing = TRUE),
               as_noteworthy("c'g' c'e'g' bd'f#' b ba a gd'g'd''"))

  notes <- "a b c,e_g' d# e_ f g"
  expect_identical(note_slice(notes, c(2:3, 6)), as_noteworthy("b c,e_g' f"))
  expect_identical(note_slice(notes, c(F, T, T, F, F, T, F)),
                   as_noteworthy("b c,e_g' f"))
  expect_identical(note_slice(notes, 6:8), as_noteworthy("f g"))
  expect_identical(note_slice(strsplit(notes, " ")[[1]], 6:8),
                   as_noteworthy(c("f", "g")))
  expect_error(
    note_slice(notes, "a"),
    "Must provide integer or logical vector index to slice `notes`.")
  expect_error(
    note_slice(notes, F),
    "Logical vector must be same length as the number of timesteps in `notes`."
    )
  expect_error(note_slice(notes, 8), "Index out of bounds.")

  y <- as_noteworthy("a r")
  expect_true(note_has_rest(y))
  expect_equal(as_space_time(y), y)
  expect_equal(as_vector_time(y), as_noteworthy(y, format = "vector"))
  expect_equal(as_space_time(as.character(y)), y)
  expect_equal(as_vector_time(as.character(y)),
               as_noteworthy(y, format = "vector"))

  expect_identical(note_rotate(notes, 3), as_noteworthy("d# e_ f g a b c,e_g'"))
  expect_identical(note_rotate(notes, 0), notes)
  expect_identical(note_rotate(notes, 3), as_noteworthy("d# e_ f g a b c,e_g'"))

  expect_identical(pretty_notes(notes), "A B CEbG D# Eb F G")
  expect_identical(pretty_notes(notes, FALSE), "A B C,EbG' D# Eb F G")

  expect_identical(note_shift("c4 e' g'", 1), as_noteworthy("e' g' c''"))
  expect_identical(note_shift("c e_ g", -4) |> as.character(), "g,, c, e_,")
  expect_identical(note_shift("c4 e_4 g4", -3) |> as.character(), "c e_ g")
  expect_identical(note_shift("a", 1), as_noteworthy("a'"))
  expect_identical(note_shift("a b ceg"), as_noteworthy("c e g a b"))
  expect_identical(note_shift(c("a", "b", "ceg")),
                   as_vector_time(as_noteworthy("c e g a b")))
  expect_identical(note_shift("a"), as_noteworthy("a"))
  expect_identical(note_shift("a", -1), as_noteworthy("a,"))

  expect_equal(note_arpeggiate("c e g") |> as.character(), "c e g")
  expect_equal(note_arpeggiate("c,,", 1) |> as.character(), "c,, c,")
  expect_equal(note_arpeggiate("c,, d,,", 1) |> as.character(),
               "c,, d,, c, d,")
  expect_equal(note_arpeggiate("c e g ceg", 1) |> as.character(),
               "c e g ceg c' e' g' c'e'g'")
  expect_equal(note_arpeggiate("c e g ceg", 1, -12) |> as.character(),
               "c e g ceg c, e, g, c,e,g,")
  expect_equal(note_arpeggiate("gec", 2, -1) |> as.character(),
               "gec g_e_b, fdb_,")
  expect_equal(note_arpeggiate("c# e g", 2, 1) |> as.character(),
               "c# e g d f g# d# f# a")
  expect_equal(note_arpeggiate("c e g", 2) |> as.character(),
               "c e g c' e' g' c'' e'' g''")
  expect_equal(note_arpeggiate("c2 d#2 g2", 1, -2) |> as.character(),
               "c2 d#2 g2 a#1 c#2 f2")
  expect_error(note_arpeggiate("a", -1), "`n` cannot be negative.")

  expect_equal(sharpen_flat("a,") |> as.character(), "a,")
  expect_equal(sharpen_flat("a_,") |> as.character(), "g#,")
  expect_equal(flatten_sharp("a#2") |> as.character(), "b_2")

  expect_equal(naturalize(notes) |> as.character(),
               "a b c,eg' d e f g")
  expect_equal(naturalize(notes, "flat") |> as.character(),
               "a b c,eg' d# e f g")
  expect_equal(naturalize(notes, "sharp") |> as.character(),
               "a b c,e_g' d e_ f g")

  expect_equal(note_set_key(notes, "f") |> as.character(),
               "a b c,e_g' e_ e_ f g")
  expect_equal(note_set_key(notes, "g") |> as.character(),
               "a b c,d#g' d# d# f g")

  x <- "a# b_ c, d'' e3 g_4 A m c2e_2g2 cegh"
  expect_equal(is_note(x), c(rep(TRUE, 6), rep(FALSE, 4)))
  expect_equal(is_chord(x), c(rep(FALSE, 8), TRUE, FALSE))
  expect_false(noteworthy(x))
  x <- strsplit(x, " ")[[1]][c(1:6, 9)]
  expect_true(noteworthy(x))

  expect_equal(is_diatonic("a a_ a# b c"), c(TRUE, FALSE, FALSE, TRUE, TRUE))

  y <- "a# b_ c, d'' e3 g_4 c2e_2g2"
  y <- as_noteworthy(y)
  expect_is(y, "noteworthy")
  expect_is(summary(y), "NULL")
  expect_is(summary(as_noteworthy("a_,*2")), "NULL")
  expect_is(summary(as_noteworthy("a#2*4")), "NULL")
  expect_is(summary(as_noteworthy("a_*2 a#*3")), "NULL")
  expect_is(print.noteworthy("a*1"), "NULL")
  expect_is(print.noteworthy(c("a", "a")), "NULL")

  y <- as_noteworthy(y, "tick", "sharp", "vector")
  expect_identical(
    y, as_noteworthy(c("a#*2", "c,", "d''", "e", "f#'", "c,d#,g,")))
  expect_error(as_noteworthy("a", format = "a"),
               "`format` must be 'space' or 'vector' if not NULL.")
  expect_error(as_noteworthy("a", octaves = "a"),
               "`octaves` must be 'tick' or 'integer' if not NULL.")
  expect_error(as_noteworthy("a", accidentals = "a"),
               "`accidentals` must be 'flat' or 'sharp' if not NULL.")
  x <- x[1:6]
  expect_equal(note_is_natural(x), c(F, F, T, T, T, F))
  expect_identical(note_is_natural(x), !note_is_accidental(x))
  expect_equal(note_is_flat(x), c(F, T, F, F, F, T))
  expect_equal(note_is_sharp(x), c(T, rep(F, 5)))
  expect_true(note_has_accidental(x))
  expect_true(note_has_natural(x))

  x <- "e_2 a_, c#f#a#'"
  expect_identical(note_set_key(x, "f"), note_set_key(x, "flat"))
  expect_identical(note_set_key(x, "g"), as_noteworthy("d#, g#, c#f#a#'"))
  expect_identical(as_tick_octaves(x), as_noteworthy("e_, a_, c#f#a#'"))
  expect_identical(as_integer_octaves(x), as_noteworthy("e_2 a_2 c#f#a#4"))
  y <- as_vector_time(x)
  expect_equal(length(y), 3)
  expect_identical(as_space_time(y), as_noteworthy(x))
  expect_true(is_space_time(as_space_time(y)))

  expect_error(note_rotate("a b x"), "Invalid notes or chords found.")
})

test_that("Note metadata inspection works", {
  x <- "e_2 a_, c#f#a#"
  expect_identical(n_steps(x), 3L)
  expect_identical(n_notes(x), 2L)
  expect_identical(n_chords(x), 1L)
  expect_equal(octave_type(x), "tick")
  expect_equal(accidental_type(x), "flat")
  expect_equal(time_format(x), "space-delimited time")

  expect_error(accidental_type("z"),
               "Cannot coerce string to 'noteworthy' or 'music'.")

  x <- "e_2 a_, b2 c c' c''g'' c''g'' c#f#a#"
  expect_identical(n_steps(x), 8L)
  expect_identical(n_notes(x), 5L)
  expect_identical(n_chords(x), 3L)
  expect_identical(n_octaves(x), 4L)

  expect_equal(nrow(tally_notes(x)), 8)
  expect_equal(nrow(tally_pitches(x)), 10)
  expect_equal(nrow(tally_octaves(x)), 4)

  expect_identical(n_notes(distinct_notes(x)), 8L)
  expect_identical(n_notes(distinct_pitches(x)), 10L)
  expect_identical(distinct_octaves(x), 2:5)

  expect_identical(pitch_range(x), c("e_,", "g''"))
  expect_identical(pitch_range("e_2"), c("e_2", "e_2"))
  expect_identical(semitone_range(x), c(39L, 79L))
  expect_identical(semitone_span(x), 40L)
  expect_identical(octave_range(x), c(2L, 5L))
  expect_identical(octave_span(x), 3L)
  expect_true(is_space_time("a"))
  expect_true(is_vector_time(c("a", "a")))
})

test_that("note equivalence functions return as expected", {
  x <- "b_2 ce_g"
  y <- "b_ cd#g"
  expect_equal(note_is_equal(x, y), c(T, T))
  expect_equal(note_is_identical(x, y), c(T, F))

  expect_equal(note_is_identical("a", "a a"), NA)

  x <- "b_2 ce_g"
  y <- "b_2 cd#g"
  expect_equal(pitch_is_equal(x, y), c(T, T))
  expect_equal(pitch_is_identical(x, y), c(T, F))

  expect_equal(pitch_is_equal("a", "a a"), NA)
  expect_equal(pitch_is_identical("a", "a a"), NA)

  x <- "b_2 ce_g b_"
  y <- "b_2 ce_gb_"
  expect_equal(note_is_equal(x, y), NA)

  x <- "b_2 ce_g b_"
  y <- "b_2 ce_ gb_"
  expect_equal(note_is_equal(x, y), c(T, F, F))

  x <- "a1 b_2 a1b2c3 a1b4 g1a1b1"
  y <- "a_2 g#2 d1e1f2g3 a1b2b4 d1e1"
  expect_equal(octave_is_equal(x, y), c(F, T, T, T, T))
  expect_equal(octave_is_identical(x, y), c(F, T, T, F, T))
  expect_equal(octave_is_identical(x, y, single_octave = TRUE),
               c(F, T, F, F, T))
  expect_equal(octave_is_identical("a1 a1", "b1 b2", single_octave = TRUE),
               c(TRUE, FALSE))
  expect_equal(
    octave_is_identical("a2c2 a2c2", "b2d2 b1d2", single_octave = TRUE),
    c(TRUE, FALSE))

  expect_equal(octave_is_equal("a", "a a"), NA)
  expect_equal(octave_is_identical("a", "a a"), NA)

  x <- c("b_2", "ce_g")
  y <- c("b_", "cd#g")
  expect_equal(note_is_equal(x, y), c(T, T))
  expect_equal(note_is_identical(x, y), c(T, F))

  x <- c("b_2", "ce_g")
  y <- c("b_2", "cd#g")
  expect_equal(pitch_is_equal(x, y), c(T, T))
  expect_equal(pitch_is_identical(x, y), c(T, F))

  x <- c("b_2", "ce_g", "b_")
  y <- c("b_2", "ce_gb_")
  expect_equal(note_is_equal(x, y), NA)

  x <- c("b_2", "ce_g", "b_")
  y <- c("b_2", "ce_", "gb_")
  expect_equal(note_is_equal(x, y), c(T, F, F))

  x <- c("a,,", "b_,", "a,,b,c3", "a,,b'", "g,,a,,b,,")
  y <- c("a_2", "g#2", "d1e1f2g3", "a1b2b4", "d1e1")
  expect_equal(octave_is_equal(x, y), c(F, T, T, T, T))
  expect_equal(octave_is_identical(x, y), c(F, T, T, F, T))
  expect_equal(octave_is_identical(x, y, single_octave = TRUE),
               c(F, T, F, F, T))
})

test_that("note_ngram returns as expected", {
  x <- as_noteworthy("a, ceg")
  y <- note_ngram(x)
  expect_equal(length(y), 2)
  expect_equal(sapply(y, length), c(1, 2))

  y <- note_ngram(x, tally = TRUE)
  expect_equal(dim(y), c(2, 2))

  expect_error(note_ngram("a", n = 0),
               "`n` must be >= 1 and <= number of timesteps.")
})
