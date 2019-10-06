context("chords")

library(dplyr)

test_that("chord helpers return as expected", {
  expect_equal(chord_invert("ab", 0), "ab")
  expect_error(chord_invert("a", 1), "Invalid chord found.")
  expect_equal(chord_invert("ceg", 1), as_noteworthy("egc4"))
  expect_equal(chord_invert("ceg", 2), as_noteworthy("gc4e4"))
  expect_equal(chord_invert("e_2g#3b_4", 1), as_noteworthy("g#3b_4e_5"))
  expect_equal(chord_invert("e_2g#3b_4", 4), as_noteworthy("g#5b_5e_6"))
  expect_equal(chord_invert("e_2g#3b_4", -4), as_noteworthy("b_0e_1g#1"))
  expect_equal(chord_invert("e_,g#3b_4", -4), as_noteworthy("b_0e_1g#1"))
  expect_equal(chord_invert("a,b,", 0), "a,b,")
  expect_equal(chord_invert("a'b'", 0), "a'b'")

  expect_error(chord_invert("ace ace"),
               "`x` must be a single chord, not space-delimited chords.")
  expect_error(
    chord_invert("abc", 3, limit = TRUE),
    "Chord has 3 notes. `n` must be in -2:2. Set `limit = FALSE` to override.")

  expect_equal(chord_break("c e g ceg ceg"), as_noteworthy("c e g c e g c e g"))
  expect_equal(chord_break(c("c", "ceg")) %>% as.character(), c("c", "c e g"))

  expect_equal(chord_is_diatonic("ceg ace ce_g", "c"), c(TRUE, TRUE, FALSE))
  expect_equal(chord_is_diatonic(c("dfa", "df#a"), "d"), c(FALSE, TRUE))

  expect_equal(chord_arpeggiate("ce_gb_", 2),
               as_noteworthy(c("ce_gb_", "e_gb_c4", "gb_c4e_4")))
  expect_equal(chord_arpeggiate("ce_gb_", -2),
               as_noteworthy(c("ce_gb_", "b_2ce_g", "g2b_2ce_")))
  expect_equal(chord_arpeggiate("ce_gb_", 2, by = "chord"),
               as_noteworthy(c("ce_gb_", "c4e_4g4b_4", "c5e_5g5b_5")))
  expect_equal(chord_arpeggiate("ce_gb_", 1, broken = TRUE, collapse = TRUE),
               as_noteworthy("c e_ g b_ e_ g b_ c4"))
  expect_equal(chord_arpeggiate("ceg"), as_noteworthy("ceg"))
  expect_equal(chord_arpeggiate("ceg'", 1),
               as_noteworthy("ceg' eg'c''", "vector"))
  expect_error(chord_arpeggiate("ceg ceg"), "`chord` must be a single chord.")

  x <- "c cg, ce ce_ ceg ce_gb g,ce g,ce_ e_,g,c e_,g,ce_ e_,g,c"
  expect_equal(chord_is_major(x), c(NA, NA, T, F, T, F, T, F, T, T, T))
  expect_identical(chord_is_major(x), !chord_is_minor(x))
  expect_identical(chord_is_minor("a"), NA)
  expect_identical(chord_is_minor("ab"), NA)
  expect_true(chord_is_minor("ce_g"))
  expect_false(chord_is_minor("ceg"))
  expect_true(chord_is_minor("ce_gb_"))
  expect_false(chord_is_minor("cegb"))
  expect_true(chord_is_minor("ce_c'e'"))
  expect_false(chord_is_minor("cec'e_'"))
  expect_equal(chord_is_minor("cgd'"), NA)
})

test_that("interval_semitones returns as expected", {
  x <- c("minor third", "m3", "augmented second", "A2")
  y <- c("P1", "m2", "M2", "m3", "M3", "P4", "TT", "P5")
  expect_equal(interval_semitones(x), rep(3, length(x)))
  expect_equal(interval_semitones(y), 0:7)

  expect_error(interval_semitones(0), NA)
})

test_that("dyad constructor returns as expected", {
  expect_equal(dyad("a", 4) %>% as.character(), "ac#4")
  x <- c("minor third", "m3", "augmented second", "A2")
  y <- sapply(x, function(x) dyad("a", x))
  expect_equal(as.character(y), rep("ac4", 4))
  y <- sapply(x, function(x) dyad("c'", x, reverse = TRUE))
  expect_equal(as.character(y), rep("ac'", 4))
  x <- c("M3", "m3", "m3", "M3", "M3", "m3", "m3")
  y <- dyad(letters[c(3:7, 1, 2)], x) %>% as.character()
  expect_equal(y, c("ce", "df", "eg", "fa", "gb", "ac4", "bd4"))
  x <- c("P1", "m3", "M3", "P4", "P5", "P8", "M9")
  expect_equal(dyad("c", x) %>% as.character(),
               c("c cd# ce cf cg cc4 cd4"))
  y <- dyad("c", x, reverse = TRUE) %>% as.character()
  expect_equal(y, c("c a2c a_2c g2c f2c c2c b_1c"))
  expect_equal(dyad("d e", "m3"), as_noteworthy("df eg"))

  err <- c("Invalid `interval`.",
           "`notes` and `interval` have unequal lengths both > 1.")
  expect_error(dyad("a", "x"), err[1])
  expect_error(dyad(letters[1:3], 1:2), err[2])
})

test_that("chord rank, order and sort work as expected", {
  x <- "a2 c a2 ceg ce_g cea"
  expect_equal(chord_rank(x, "min"), c(1.5, 4.5, 1.5, 4.5, 4.5, 4.5))
  expect_equal(chord_rank(x, "max"), c(1.5, 3, 1.5, 4.5, 4.5, 6))
  expect_equal(chord_rank(x, "mean"), c(1.5, 3, 1.5, 5, 4, 6))

  expect_equal(chord_order(x), c(1, 3, 2, 4, 5, 6))
  expect_equal(chord_order(x, "mean"), c(1, 3, 2, 5, 4, 6))
  expect_equal(as.character(chord_sort(x, "mean")), "a2 a2 c ce_g ceg cea")
  expect_equal(as.character(chord_sort(x, "mean", TRUE)),
               "cea ceg ce_g c a2 a2")
})

test_that("chord root, topr and general slice work as expected", {
  x <- "a2 ceg e_gc egc,cc'"
  y <- strsplit(x, " ")[[1]]
  a <- c("a2 c c c,", "a2 g g c'", "e e_ c", "g", "g g egc'")
  b <- strsplit(a, " ")

  expect_identical(chord_root(x), as_noteworthy(a[1]))
  expect_identical(chord_top(x), as_noteworthy(a[2]))
  expect_identical(chord_slice(x, 1), chord_root(x))
  expect_identical(chord_slice(x, 2), as_noteworthy(a[3]))
  expect_identical(chord_slice(x, 4), as_noteworthy(a[4]))
  expect_identical(chord_slice(x, 3:5), as_noteworthy(a[5]))
  expect_error(chord_slice(x, 0), "Index out of bounds for all chords.")

  expect_identical(chord_root(y), as_noteworthy(b[[1]]))
  expect_identical(chord_top(y), as_noteworthy(b[[2]]))
  expect_identical(chord_slice(y, 1), chord_root(y))
  expect_identical(chord_slice(y, 2), as_noteworthy(b[[3]]))
  expect_identical(chord_slice(y, 4), as_noteworthy(b[[4]]))
  expect_identical(chord_slice(y, 3:5), as_noteworthy(b[[5]]))
  expect_error(chord_slice(y, 6), "Index out of bounds for all chords.")
})

test_that("chord constructors return as expected", {
  expect_equal(xm("c", "g") %>% as.character(), "cd#g")
  expect_equal(xm("c", "f") %>% as.character(), "ce_g")
  expect_equal(xm7("c", "f") %>% as.character(), "ce_gb_")
  expect_equal(x7("c", "f") %>% as.character(), "cegb_")
  expect_equal(x5("c"), dyad("c", "P5"))

# lintr gets confused thinking there are comment lines - # nolint start

  test_chord_constructors <- function(root, n, key, ...){
    s <- list(...)$style
    if(is.null(s)) s <- "default"
    expect_equal(xm(root, key, style = s),
                 transpose("cd#g", n, key, style = s))
    expect_equal(xM(root, key, style = s),
                 transpose("ceg", n, key, style = s))
    expect_equal(xm7(root, key, style = s),
                 transpose("cd#ga#", n, key, style = s))
    expect_equal(x7(root, key, style = s),
                 transpose("cega#", n, key, style = s))
    expect_equal(x7s5(root, key, style = s),
                 transpose("ceg#a#", n, key, style = s))
    expect_equal(xM7(root, key, style = s),
                 transpose("cegb", n, key, style = s))
    expect_equal(xm6(root, key, style = s),
                 transpose("cd#ga", n, key, style = s))
    expect_equal(xM6(root, key, style = s),
                 transpose("cega", n, key, style = s))
    expect_equal(xdim(root, key, style = s),
                 transpose("cd#f#", n, key, style = s))
    expect_equal(xdim7(root, key, style = s),
                 transpose("cd#f#a", n, key, style = s))
    expect_equal(xm7b5(root, key, style = s),
                 transpose("cd#f#a#", n, key, style = s))
    expect_equal(xaug(root, key, style = s),
                 transpose("ceg#", n, key, style = s))
    expect_equal(x5(root, key, style = s),
                 transpose("cg", n, key, style = s))
    expect_equal(xs2(root, key, style = s),
                 transpose("cdg", n, key, style = s))
    expect_equal(xs4(root, key, style = s),
                 transpose("cfg", n, key, style = s))
    expect_equal(x9(root, key, style = s),
                 transpose("cega#d4", n, key, style = s))
    expect_equal(x7s9(root, key, style = s),
                 transpose("cega#d#4", n, key, style = s))
    expect_equal(xM9(root, key, style = s),
                 transpose("cegbd4", n, key, style = s))
    expect_equal(xadd9(root, key, style = s),
                 transpose("cegd4", n, key, style = s))
    expect_equal(xm9(root, key, style = s),
                 transpose("cd#ga#d4", n, key, style = s))
    expect_equal(xma9(root, key, style = s),
                 transpose("cd#gd4", n, key, style = s))
    expect_equal(xm11(root, key, style = s),
                 transpose("cd#ga#d4f4", n, key, style = s))
    expect_equal(x7s11(root, key, style = s),
                 transpose("cega#f#4", n, key, style = s))
    expect_equal(xM7s11(root, key, style = s),
                 transpose("cegbd4f#4", n, key, style = s))
    expect_equal(x_11(root, key, style = s),
                 transpose("cga#d4f4", n, key, style = s))
    expect_equal(xM11(root, key, style = s),
                 transpose("cegbd4f4", n, key, style = s))
    expect_equal(x_13(root, key, style = s),
                 transpose("cega#d4a4", n, key, style = s))
    expect_equal(xm13(root, key, style = s),
                 transpose("cd#ga#d4a4", n, key, style = s))
    expect_equal(xM13(root, key, style = s),
                 transpose("cegbd4f4a4", n, key, style = s))
  }

  test_chord_constructors("c", 0, "c")
  test_chord_constructors("b_2", -2, "d")
  test_chord_constructors("a#,", -2, "d", style = "tick")
  test_chord_constructors("b_3", 10, "f")
  test_chord_constructors("e_1", -21, "f")
  test_chord_constructors("d#''", 27, "f", style = "tick")
  test_chord_constructors("d#''", 27, "f", style = "integer")
  test_chord_constructors("d#''", 27, "f", style = "strip")

  expect_equal(xm(c("c#", "f#", "g#")) %>% as.character(),
               c("c#eg#", "f#ac#4", "g#bd#4"))
  expect_equal(xm("b_2 f g"), as_space_time(xm(c("b_2", "f", "g"))))

# nolint end
})
