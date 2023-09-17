test_that("key helpers return as expected", {
  sharps <- c("g", "d", "a", "e", "b", "f#", "c#", "em", "bm", "f#m", "c#m",
              "g#m", "d#m", "a#m")
  flats <- c("f", "b_", "e_", "a_", "d_", "g_", "c_", "dm", "gm", "cm", "fm",
             "b_m", "e_m", "a_m")
  expect_equal(keys(), .keydata$key)
  expect_equal(keys("sharp"), sharps)
  expect_equal(keys("flat"), flats)
  expect_equal(key_is_natural(c("c", "am", "c#")), c(TRUE, TRUE, FALSE))
  x <- c("c", "am", "a", "e_")
  expect_equal(key_is_sharp(x), c(FALSE, FALSE, TRUE, FALSE))
  expect_equal(key_is_flat(x), c(FALSE, FALSE, FALSE, TRUE))
  expect_equal(key_n_sharps(x), c(0, 0, 3, 0))
  expect_equal(key_n_flats(x), c(0, 0, 0, 3))
  y <- c(TRUE, FALSE, TRUE, TRUE)
  expect_equal(key_is_major(x), y)
  expect_equal(key_is_major(x), !key_is_minor(x))
})
