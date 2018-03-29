context("outputs")

notes <- "r s a2~ a2 c3 c f4 f' d a f"
info <- "4 4 8*9"
p1 <- p(notes, info)
p2 <- p("r s a~ a c' c4 f5 f'' d4 a4 f'", info)
x1 <- track(p1) %>% score()
x2 <- track(p1, tuning = "DADGAD", ms_transpose = 1, ms_key = "flat") %>% score()
x3 <- track(p1, music_staff = NA) %>% score()

notes <- "a, b, c d e f g a"
p1 <- p(notes, 8)
p2 <- p(tp(notes, 7), 8)
p3 <- p(tp(notes, -12), 8)
x4 <- trackbind(track(p1, voice = 1),
                track(p2, voice = 2), tabstaff = c(1, 1)) %>% score()
x5 <- trackbind(track(p1), track(p3, tuning = "bass", music_staff = "bass_8")) %>% score()
x <- list(x1, x2, x3, x4, x5)

test_that("tab and lilypond functions run without error", {
  skip_on_appveyor()
  skip_on_cran()
  purrr::walk(x, ~expect_is(lilypond(.x, "out.ly"), "NULL"))
  purrr::walk(x, ~expect_is(tab(x1, "out.pdf"), "NULL"))
  purrr::walk(x, ~expect_is(tab(x1, "out.png"), "NULL"))
  unlink(c("out.mid", "out.pdf", "out.png"))
})

test_that("miditab and midily functions run without error", {
  skip_on_appveyor()
  skip_on_cran()
  midi <- system.file("example.mid", package = "tabr")
  expect_is(midily(midi, "out.ly"), "NULL")
  expect_is(miditab(midi, "out.pdf"), "NULL")
  expect_is(miditab(midi, "out.png"), "NULL")
  unlink(c("out.mid", "out.pdf", "out.png"))
})
