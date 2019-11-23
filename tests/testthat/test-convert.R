context("convert")

test_that("syntax conversions are as expected", {
  chords <- c("Bb", "Bbm", "Bbm7", "Bbm7(b5)", "Bb7(#5)/G", "Bb7(#5)/Ab")
  x <- from_chorrrds(chords)
  expect_equal(
    x,
    as_noteworthy("b_d'f' b_d_'f' b_d_'f'a_' b_d_'e'a_' gb_d'g_'a_' a_b_d'g_'")
  )
  y <- to_tabr(id = "chorrrds", x = chords)
  expect_identical(x, y)

  wrn <- "Alternate bass note detected, but ignored when `guitar = TRUE`."
  expect_warning(x <- from_chorrrds(chords, guitar = TRUE), wrn)
  expect_equal(
    x,
    as_noteworthy(
      "b_,fb_d'f' b_,fb_d_'f' b_,fa_d_'f' b_,a_d_'e' b_,g_a_d' b_,g_a_d'"
    )
  )
  expect_warning(y <- to_tabr(id = "chorrrds", x = chords, guitar = TRUE),
                 wrn)
  expect_identical(x, y)

  expect_equal(to_tabr("chorrrds", x = c("Bbm", "Bbm/F"), key = "Bbm"),
               as_noteworthy("b_d_'f' fb_d_'"))
  expect_equal(to_tabr("chorrrds", x = c("Ebm", "Ebm/Bb"), key = "Ebm"),
               as_noteworthy("e_g_b_ b_,e_g_"))
  expect_error(to_tabr("c", "a"), "Function `tabr::from_c` not found.")

  x <- "2/2 CC#FF4.. trip{c#8eg# d'- e-' f g a'} D4~# D E F r B16 B16"
  y <- from_music21(x)
  expect_equal(music_time(y), "2/2")
  expect_is(from_music21(x, accidentals = "sharp"), "music")
  z <- from_music21(x, output = "list")
  expect_identical(music_split(y), z)
  expect_error(from_music21("--"), "Double flat/sharp currently not allowed")

  z <- from_music21(strsplit(x, " ")[[1]])
  expect_identical(y, z)

  x <- gsub("2/2 ", "", x)
  y <- from_music21(x)
  expect_equal(music_time(y), "4/4")
})
