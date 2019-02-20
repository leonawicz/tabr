context("notes")

test_that("note helpers return as expected", {
  notes <- "a b c,e_g' d# e_ f g"
  expect_identical(note_rotate(notes, 0), notes)
  expect_identical(note_rotate(notes, 3), "d# e_ f g a b c,e_g'")

  expect_identical(note_shift("c4 e' g'", 1), "e' g' c''")
  expect_identical(note_shift("c e_ g", -4), "g1 c2 e_2")
  expect_identical(note_shift("c4 e_4 g4", -3), "c e_ g")

  expect_equal(note_arpeggiate("c e g", 5), "c e g c4 e4 g4 c5 e5")
  expect_equal(note_arpeggiate("c e g", -5), "e1 g1 c2 e2 g2 c e g")
  expect_equal(note_arpeggiate("c e g", 5, style = "tick"), "c e g c' e' g' c'' e''")
  expect_equal(note_arpeggiate("c e_ g", -5, key = "f"), "e_1 g1 c2 e_2 g2 c e_ g")
  expect_equal(note_arpeggiate("c e_ g", -5, key = "g"), "d#1 g1 c2 d#2 g2 c d# g")

  expect_equal(sharpen_flat("a,"), "a,")
  expect_equal(sharpen_flat("a_,"), "g#,")
  expect_equal(flatten_sharp("a#2"), "b_2")
  expect_equal(flatten_sharp("a#2", TRUE), "b_")

  expect_equal(naturalize(notes), "a b c,eg' d e f g")
  expect_equal(naturalize(notes, "flat"), "a b c,eg' d# e f g")
  expect_equal(naturalize(notes, "sharp"), "a b c,e_g' d e_ f g")
  expect_equal(naturalize(notes, strip = TRUE), "a b ceg d e f g")

  x <- "a# b_ c, d'' e3 g_4 A m c2e_2g2 cegh"
  expect_equal(is_note(x), c(rep(TRUE, 6), rep(FALSE, 4)))
  expect_equal(is_chord(x), c(rep(FALSE, 8), TRUE, FALSE))
  expect_false(noteworthy(x))
  x <- strsplit(x, " ")[[1]][c(1:6, 9)]
  expect_true(noteworthy(x))

  x <- x[1:6]
  expect_equal(note_is_natural(x), c(F, F, T, T, T, F))
  expect_identical(note_is_natural(x), !note_is_accidental(x))
  expect_equal(note_is_flat(x), c(F, T, F, F, F, T))
  expect_equal(note_is_sharp(x), c(T, rep(F, 5)))

  err <- c("Invalid note found.", "Invalid notes or chords found.")
  expect_error(note_rotate("a b x"), err[2])
  expect_error(note_shift("a b ceg"), err[1])
  expect_error(note_shift("a b ceg"), err[1])
})

test_that("note equivalence functions return as expected", {
  x <- "b_2 ce_g"
  y <- "b_ cd#g"
  expect_equal(note_is_equal(x, y), c(T, T))
  expect_equal(note_is_identical(x, y), c(T, F))

  x <- "b_2 ce_g"
  y <- "b_2 cd#g"
  expect_equal(pitch_is_equal(x, y), c(T, T))
  expect_equal(pitch_is_identical(x, y), c(T, F))

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
  expect_equal(octave_is_identical(x, y, single_octave = TRUE), c(F, T, F, F, T))

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
  expect_equal(octave_is_identical(x, y, single_octave = TRUE), c(F, T, F, F, T))
})
