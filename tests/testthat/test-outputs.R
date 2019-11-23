context("outputs")

library(dplyr)

notes <- "r s a2~ a2 c3 c f4 f' d a f ce_g ceg#"
info <- "4. 4. 4.. 4- 4..-. 8^ 8-^ 8-> 4.-! t4 t4[staccato] t4[accent] 2.."
p1 <- pc(pct(p("a", 1)), rp(p(notes, info)))
p2 <- volta(p("r s a~ a c' c4 f5 f'' d4 a4 f' c'e_'g' c'e'g#'",
              "4. 4 8*9 2. 2"))
x1 <- track(p1) %>% score()
x2 <- track(p2, tuning = "DADGAD", ms_transpose = 1, ms_key = "flat") %>%
  score()
x3 <- track(pc(p1, p2), music_staff = NA) %>% score()

notes <- "a, b, c d e f g a"
p1 <- p(notes, 8)
p2 <- p(tp(notes, 7), 8)
p3 <- p(tp(notes, -12), 8)
x4 <- trackbind(track(p1, voice = 1),
                track(p2, voice = 2), tabstaff = c(1, 1)) %>% score()

chord_prep <- c("b_:m" = "x13321", "c/g" = "332o1o",
                "b:m/f#" = "14 12 14 14 13 12", "r" = NA, "s" = NA)
chords <- chord_set(chord_prep)

test_that("chord_set returns as expected", {
  expect_equal(names(chords), names(chord_prep))
  expect_equal(chord_set(c("b_:m" = "x;1;3;3;2;1;")),
               chord_set(c("b_:m" = "x 1 3 3 2 1")))
  expect_error(
    chord_set(c("b_:m" = "x 1 3 3 2 1"), n = 5),
    "Cannot have more fret values than number of instrument strings.")
})

chord_seq <- setNames(c(4, 4, 2), names(chords[1:3]))
t1 <- trackbind(track(p1), track(p3, tuning = "bass", music_staff = "bass_8"))

x5 <- score(t1)
x6 <- score(t1, chords)
x7 <- score(t1, chord_seq = chord_seq)
x8 <- score(t1, chords, chord_seq)

x9 <- trackbind(
  track(p1, voice = 1, no_tab = TRUE),
  track(p2, voice = 2, no_tab = TRUE),
  track(p3, tuning = "bass", music_staff = NA),
  track(p3, tuning = "bass", music_staff = "bass_8", no_tab = TRUE),
  tabstaff = c(1, 1, 2, 3)
  ) %>%
  score()

x10 <- trackbind(
  track(p3, tuning = "bass", ms_transpose = 1, ms_key = "sharp"),
  track(p3, tuning = "bass", music_staff = "bass_8", ms_transpose = 2,
        ms_key = "sharp", no_tab = TRUE)
) %>%
  score()

x <- list(x1, x2, x3, x4, x5, x6, x7, x8, x9)

header <- list(
  title = "my title", subtitle = "subtitle", composer = "composer",
  album = "album", arranger = "arranger", instrument = "instrument",
  meter = "meter", opus = "opus", piece = "piece", poet = "poet",
  copyright = "copyright", tagline = "tagline")
paper <- list(textheight = 230, linewidth = 160, indent = 20, fontsize = 16,
              first_page_number = 10, page_numbers = FALSE)

out <- file.path(tempdir(), c("out.ly", "out.pdf", "out.png", "out.mid"))
cleanup <- file.path(tempdir(), c("out.mid", "out.pdf", "out.png", "out.log",
                                  "out.ly"))
cl <- "NULL"

test_that("track wrappers return as expected", {
  z <- phrase("a", 1)
  expect_false(track_tc(z)$tab)
  expect_equal(track_bc(z)$staff, "bass")
  expect_equal(track_bass(z)$staff, "bass_8")
})

test_that("lilypond wrapper runs without error", {
  skip_on_appveyor()
  expect_is(lilypond(x1, out[1]), cl)
  expect_is(lilypond(x2, out[1]), cl)
  purrr::walk(x, ~expect_is(lilypond(.x, out[1]), cl))
  purrr::walk(x, ~expect_is(lilypond(.x, out[1], simplify = FALSE), cl))
  expect_warning(lilypond(x10, out[1]),
                 paste("Multiple music staves with different transposed key",
                       "signatures. MIDI output not transposed."))
  expect_error(lilypond(NULL, tempo = NULL, midi = TRUE),
               "Set an explicit `tempo` if `midi = TRUE`.")
  expect_is(lilypond(x1, out[1], tempo = NULL, midi = FALSE), cl)
  expect_is(lilypond(x1, out[1], header = list(title = "Title"),
                     colors = list(x = 2)), cl)
  expect_is(lilypond(x1, out[1], header = list(title = "Title"),
                     colors = list(color = "#FF3030", staff = "red", x = 2)),
            cl)
  expect_error(lilypond(x1, out[1], paper = list(print_first_page_number = 1)),
               "`print_first_page_number` must be logical.")
  expect_error(lilypond(x1, out[1], paper = list(page_numbers = 1)),
               "`page_numbers` must be logical.")
  expect_error(lilypond(x1, out[1], paper = list(first_page_number = TRUE)),
               "`first_page_number` must be a number.")
  unlink(cleanup)
})

test_that("tab wrapper runs without error", {
  skip_on_appveyor()
  skip_on_cran()
  include_midi <- TRUE
  expect_is(tab(x1, out[2], midi = include_midi, details = TRUE), cl)
  expect_is(tab(x1, out[3], midi = include_midi), cl)
  expect_is(tab(x1, out[3], midi = include_midi, transparent = TRUE), cl)
  expect_is(render_tab(x1, out[3], midi = include_midi, transparent = TRUE), cl)
  expect_is(
    render_score(x1, out[3], transparent = TRUE,
                 colors = list(color = "blue", background = "#888888")), cl)
  expect_is(render_midi(x1, out[4]), cl)

  purrr::walk(x, ~expect_is(
    tab(.x, out[2], midi = include_midi), cl))
  purrr::walk(x, ~expect_is(
    tab(.x, out[3], midi = include_midi), cl))
  purrr::walk(x, ~expect_is(
    tab(.x, out[3], midi = FALSE, simplify = FALSE), cl))

  expect_error(tab(x8, out[2], "d_m", midi = include_midi),
               "Invalid `key`. See `keys\\(\\)`.")

  purrr::walk(keys(), ~expect_is(
    tab(x8, out[2], .x, "2/2", "4 = 110", header = header,
        string_names = FALSE, paper = paper, endbar = FALSE, midi = FALSE,
        keep_ly = FALSE), cl))
  purrr::walk(keys(), ~expect_is(
    tab(x8, out[2], .x, "4/4", "4 = 120", header = header,
        string_names = TRUE, paper = paper, midi = FALSE,
        keep_ly = FALSE, simplify = FALSE), cl))

  expect_is(tab(x8, out[2], "c#", "2/2", "4 = 110",
                header = c(header[c(1, 3, 4)], metre = "meter"),
                string_names = TRUE, paper = paper[1:5], endbar = FALSE,
                midi = FALSE, keep_ly = FALSE), cl)
  unlink(cleanup)
})

test_that("miditab and midily functions run without error", {
  skip_on_appveyor()
  skip_on_cran()
  midi <- system.file("example.mid", package = "tabr")
  expect_is(midily(midi, out[1]), cl)
  expect_is(midily(midi, out[1], key = "b_", absolute = TRUE, quantize = 8,
                   explicit = TRUE, start_quant = 8, allow_tuplet = "8*2/3",
                   lyric = TRUE), cl)
  expect_is(midily(midi, out[1], key = "cm", absolute = TRUE, quantize = 8,
                   explicit = TRUE, start_quant = 8, allow_tuplet = "8*2/3",
                   lyric = TRUE), cl)
  expect_is(miditab(midi, out[2], details = TRUE), cl)
  expect_is(miditab(midi, out[3]), cl)
  unlink(cleanup)
})

test_that("render_chordchart runs without error", {
  skip_on_appveyor()
  skip_on_cran()
  chords <- filter(
    guitarChords, root %in% c("c", "f") & id %in% c("7", "M7", "m7") &
    !grepl("#", notes) & root_fret <= 12) %>%
    arrange(root, id)
  chords <- setNames(chords$fretboard, chords$lp_name)

  expect_is(render_chordchart(chords, out[2], 2, paper = list(fontsize = 16),
                              header = list(title = "A title"), details = TRUE),
            cl)
  expect_is(render_chordchart(chords, out[3], 1, keep_ly = TRUE), cl)
  expect_is(render_chordchart(chords, out[3], 1, transparent = TRUE,
                              colors = list(color = "blue")), cl)
  unlink(cleanup)
})
