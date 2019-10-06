context("notes")

library(dplyr)

test_that("note helpers return as expected", {
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

  expect_identical(note_rotate(notes, 3), as_noteworthy("d# e_ f g a b c,e_g'"))
  expect_identical(note_rotate(notes, 0), notes)
  expect_identical(note_rotate(notes, 3), as_noteworthy("d# e_ f g a b c,e_g'"))

  expect_identical(pretty_notes(notes), "A B CEbG D# Eb F G")
  expect_identical(pretty_notes(notes, FALSE), "A B C,EbG' D# Eb F G")

  expect_identical(note_shift("c4 e' g'", 1), as_noteworthy("e' g' c''"))
  expect_identical(note_shift("c e_ g", -4) %>% as.character(), "g1 c2 e_2")
  expect_identical(note_shift("c4 e_4 g4", -3) %>% as.character(), "c e_ g")
  expect_identical(note_shift("a", 1), as_noteworthy("a4"))

  expect_equal(note_arpeggiate("c e g") %>% as.character(), "c e g")
  expect_equal(note_arpeggiate("c,,", 1) %>% as.character(), "c,, c,")
  expect_equal(note_arpeggiate("c,, d,,", 1) %>% as.character(), "c,, d,, c,")
  expect_equal(note_arpeggiate("c e g", 5) %>% as.character(),
               "c e g c4 e4 g4 c5 e5")
  expect_equal(note_arpeggiate("c e g", -5) %>% as.character(),
               "e1 g1 c2 e2 g2 c e g")
  expect_equal(note_arpeggiate("c e g", 5, style = "tick") %>% as.character(),
               "c e g c' e' g' c'' e''")
  expect_equal(note_arpeggiate("c e_ g", -5, key = "f") %>% as.character(),
               "e_1 g1 c2 e_2 g2 c e_ g")
  expect_equal(note_arpeggiate("c e_ g", -5, key = "g") %>% as.character(),
               "d#1 g1 c2 d#2 g2 c d# g")

  expect_equal(sharpen_flat("a,") %>% as.character(), "a,")
  expect_equal(sharpen_flat("a_,") %>% as.character(), "g#,")
  expect_equal(flatten_sharp("a#2") %>% as.character(), "b_2")
  expect_equal(flatten_sharp("a#2", TRUE) %>% as.character(), "b_")

  expect_equal(naturalize(notes) %>% as.character(),
               "a b c,eg' d e f g")
  expect_equal(naturalize(notes, "flat") %>% as.character(),
               "a b c,eg' d# e f g")
  expect_equal(naturalize(notes, "sharp") %>% as.character(),
               "a b c,e_g' d e_ f g")
  expect_equal(naturalize(notes, ignore_octave = TRUE) %>% as.character(),
               "a b ceg d e f g")

  expect_equal(note_set_key(notes, "f") %>% as.character(),
               "a b c,e_g' e_ e_ f g")
  expect_equal(note_set_key(notes, "g") %>% as.character(),
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

  y <- as_noteworthy(y, "vector", "tick", "sharp")
  expect_identical(
    y, as_noteworthy(c("a#*2", "c,", "d''", "e", "f#'", "c,d#,g,")))
  expect_error(as_noteworthy("a", "a"),
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

  x <- "e_2 a_, c#f#a#'"
  expect_identical(note_set_key(x, "f"), note_set_key(x, "flat"))
  expect_identical(note_set_key(x, "g"), as_noteworthy("d#, g#, c#f#a#'"))
  expect_identical(as_tick_octaves(x), as_noteworthy("e_, a_, c#f#a#'"))
  expect_identical(as_integer_octaves(x), as_noteworthy("e_2 a_2 c#f#a#4"))
  y <- as_vector_time(x)
  expect_equal(length(y), 3)
  expect_identical(as_space_time(y), as_noteworthy(x))
  expect_true(is_space_time(as_space_time(y)))

  err <- c("Invalid note found.", "Invalid notes or chords found.")
  expect_error(note_rotate("a b x"), err[2])
  expect_error(note_shift("a b ceg"), err[1])
  expect_error(note_shift("a b ceg"), err[1])
})

test_that("Note metadata inspection works", {
  x <- "e_2 a_, c#f#a#"
  expect_identical(n_steps(x), 3L)
  expect_identical(n_notes(x), 2L)
  expect_identical(n_chords(x), 1L)
  expect_equal(octave_type(x), "ambiguous")
  expect_equal(accidental_type(x), "both/ambiguous")
  expect_equal(time_format(x), "space-delimited time")

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

  expect_identical(pitch_range(x), c("e_2", "g''"))
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
  expect_equal(octave_is_identical("a1 a2", "b1 b1", single_octave = TRUE),
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
