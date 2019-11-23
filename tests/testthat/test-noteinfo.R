context("note info")

test_that("expect note info helpers return as expected", {
  a <- notate("8x", "Start here")
  x <- paste(a, "8-. 8-. 16 4.. 16- 16 2^ 2 4. 8( 4)( 4) 8*4 1 1")

  expect_true(informable(x))
  expect_false(informable("3"))
  expect_false(informable("r"))
  expect_false(informable("4a"))
  y <- as_noteinfo(x)
  expect_identical(y, as_noteinfo(y))
  expect_true(informable(y))
  expect_true(informable(c(NA, 2), na.rm = TRUE))
  expect_identical(string_fold(y), x)
  expect_true(is_noteinfo(y))
  expect_is(print.noteinfo(y), "NULL")
  expect_is(summary.noteinfo(y), "NULL")
  expect_false(informable(character(0)))

  expect_true(is_space_time(y))
  expect_false(is_vector_time(y))
  expect_true(is_space_time(as.character(y)))
  expect_false(is_vector_time(as.character(y)))
  expect_equal(as_space_time(y), y)
  expect_equal(as_vector_time(y), as_noteinfo(y, "vector"))
  expect_equal(as_space_time(as.character(y)), y)
  expect_equal(as_vector_time(as.character(y)), as_noteinfo(y, "vector"))

  expect_error(.check_noteinfo("3"), "Invalid note info found.")

  a <- notate("8x", "Start here")
  notes <- "a, b, c d e f g# a r ac'e' a c' e' c' r*3 ac'e'~ ac'e'"
  info <- paste(a, "8x 8-. 16 4.. 16- 16 2^ 2 4. 8( 4)( 4) 8*4 1 1")
  x <- phrase(notes, info)

  expect_is(info_duration(x), "noteinfo")
  expect_equal(sum(info_slur_on(x)), 2)
  expect_equal(sum(info_slur_off(x)), 2)
  expect_true(info_slide(x)[6])
  expect_equal(sum(info_dotted(x)), 2)
  expect_equal(sum(info_single_dotted(x)), 1)
  expect_equal(sum(info_double_dotted(x)), 1)
  a <- info_annotation(x)
  expect_true(all(is.na(a[-1])))
  expect_equal(a[1], "Start here")

  expect_equal(n_steps("1( 4."), 2)
  expect_error(n_steps("1( 5."),
               paste("Cannot coerce string to any of class",
               "'noteworthy', 'noteinfo', or 'music'."))
  expect_equal(n_steps(4), 1)
})
