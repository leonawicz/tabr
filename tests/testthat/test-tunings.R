context("tunings")

test_that("tunings and mappings work", {
  expect_identical(.map_tuning(tunings$id[5]), tunings$value[5])
  err <- "Invalid `tuning`."
  purrr::walk(c("dropd", "dadgad", "A", " h"), ~expect_error(.map_tuning(.x), err))
})
