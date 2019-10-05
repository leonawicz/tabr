context("tunings")

test_that("tunings and mappings work", {
  standard <- "e, a, d g b e'"
  x <- c(standard, "e2 a2 d3 g3 b3 e4", "e2 a, d g b e'", "standard")
  purrr::walk(x, ~expect_identical(.map_tuning(.x), standard))
  expect_identical(.map_tuning(tunings$id[5]), tunings$value[5])
  err <- "Invalid `tuning`."
  purrr::walk(c("dropd", "dadgad", "A", " h"),
              ~expect_error(.map_tuning(.x), err))
})
