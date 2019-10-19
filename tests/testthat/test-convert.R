context("convert")

test_that("syntax conversions are as expected", {
  chords <- c("Bb", "Bbm", "Bbm7", "Bbm7(b5)", "Bb7(#5)/G", "Bb7(#5)/Ab")
  x <- from_chorrrds(chords)
  expect_equal(
    x,
    as_noteworthy("b_d'f' b_d_'f' b_d_'f'a_' b_d_'e'a_' gb_d'g_'a_' a_b_d'g_'")
  )
  y <- to_tabr(id = "chorrrds", chords = chords)
  expect_identical(x, y)

  wrn <- "Alternate bass note detected, but ignored when `guitar = TRUE`."
  expect_warning(x <- from_chorrrds(chords, guitar = TRUE), wrn)
  expect_equal(
    x,
    as_noteworthy(
      "b_,fb_d'f' b_,fb_d_'f' b_,fa_d_'f' b_,a_d_'e' b_,g_a_d' b_,g_a_d'"
    )
  )
  expect_warning(y <- to_tabr(id = "chorrrds", chords = chords, guitar = TRUE),
                 wrn)
  expect_identical(x, y)

  expect_equal(to_tabr("chorrrds", chords = c("Bbm", "Bbm/F"), key = "Bbm"),
               as_noteworthy("b_d_'f' fb_d_'"))
  expect_equal(to_tabr("chorrrds", chords = c("Ebm", "Ebm/Bb"), key = "Ebm"),
               as_noteworthy("e_g_b_ b_,e_g_"))
  expect_error(to_tabr("c", "a"), "Function `tabr::from_c` not found.")
})
