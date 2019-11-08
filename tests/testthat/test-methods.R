context("methods")

test_that("noteworthy logical operators return as expected", {
  x <- as_noteworthy("f# a d'f#'a' d'f#'a'")
  y <- as_noteworthy("g_ b f#'a'd' d'd''")
  expect_identical(x == y, c(TRUE, FALSE, TRUE, FALSE))
  expect_identical(x != y, !(x == y))
  expect_identical(x < y, c(FALSE, TRUE, FALSE, TRUE))
  expect_identical(x > y, rep(FALSE, 4))
  expect_identical(x <= y, !(x > y))
  expect_identical(x >= y, !(x < y))
})

test_that("other noteworthy methods return as expected", {
  x <- as_noteworthy("a, b, c ce_g d4f#4a4")
  expect_equal(as.character(x[3:4]), "c ce_g")
  expect_equal(as.character(x[-2]), "a, c ce_g d'g_'a'")
  x[2] <- paste0(transpose(x[2], 1), "~")
  expect_equal(as.character(x), "a, c~ c ce_g d'g_'a'")

  x[[2]] <- "b_"
  expect_equal(x[[2]], "b_")

  x <- as_noteworthy("a b c")
  expect_equal(length(x), 3)
  expect_equal(length(as_noteworthy("a b*2 c*2")), 5)

  expect_equal(as.character(c(x, x)), "a b c a b c")
  expect_identical(c(x, as.character(x)), c(x, x))
  expect_identical(c(x, x), rep(x, 2))
  expect_equal(as.character(rep(as_noteworthy("a b"), each = 2)), "a a b b")
  expect_equal(as.character(rep(as_noteworthy("a b"), times = 1:2)), "a b b")

  expect_identical(rev(as_noteworthy("a b")), as_noteworthy("b a"))
  expect_equal(head(as_noteworthy("a b c d e f g")),
               as_noteworthy("a b c d e f"))
  expect_equal(tail(as_noteworthy("a b c d e f g")),
               as_noteworthy("b c d e f g"))
})

test_that("noteinfo methods return as expected", {
  x <- as_noteinfo(c("4-", "t8(", "t8)", "t8x", "8^", "16"))
  expect_equal(as.character(x[2:4]), c("t8(", "t8)", "t8x"))
  expect_equal(length(x[-1]), length(x) - 1)
  x[5:6] <- c("16^", "8")
  expect_equal(x[5:6], as_noteinfo(c("16^", "8")))
  expect_equal(as.character(x[x == "4-"]), "4-")

  x[[2]] <- "32"
  expect_equal(x[[2]], "32")

  x <- as_noteinfo("4- t8( t8)( t8) 4*2")
  expect_equal(length(x), 6)

  expect_equal(as.character(c(x, x)), "4- t8( t8)( t8) 4 4 4- t8( t8)( t8) 4 4")
  expect_identical(c(x, as.character(x)), c(x, x))
  expect_identical(c(x, x), rep(x, 2))
  expect_equal(as.character(rep(as_noteinfo("t8x 8"), each = 2)), "t8x t8x 8 8")
  expect_equal(as.character(rep(as_noteinfo("t8x 8"), times = 1:2)), "t8x 8 8")

  expect_identical(rev(as_noteinfo("1 2")), as_noteinfo("2 1"))
  expect_equal(head(as_noteinfo("4 4 4 4 8 8 8 8")), as_noteinfo("4 4 4 4 8 8"))
  expect_equal(tail(as_noteinfo("4 4 4 4 8 8 8 8")), as_noteinfo("4 4 8 8 8 8"))
})

test_that("music methods return as expected", {
  x <- as_music("c,~4 c,1 c'e_'g'4]*4")
  expect_equal(as.character(x[1:3]), "c,~4 c,1 c'e_'g'4]")
  expect_equal(length(x[-c(1:2)]), 4)
  x[3:6] <- "c'e'g'8"
  expect_equal(as.character(x[3:6]), string_unfold("c'e'g'8*4"))

  x[[2]] <- "b_4"
  expect_equal(x[[2]], "b_4")

  x <- as_music("c,~4 c,1 c'e_'g'4]*4")
  y <- "c,~4 c,1 c'e_'g'4] c'e_'g'4] c'e_'g'4] c'e_'g'4]"
  expect_equal(length(x), 6)

  expect_equal(as.character(c(x, x)), paste(y, y, sep = " "))
  expect_identical(c(x, as.character(x)), c(x, x))
  expect_identical(c(x, x), rep(x, 2))
  expect_warning(z <- c(as_music("a4", key = "a"), as_music("a4")),
                 "Key signature is inconsistent. Only the first is used.")
  expect_equal(music_key(z), "a")
  expect_warning(z <- c(as_music("a4", time = "3/4"), as_music("a4")),
                 "Time signature is inconsistent. Only the first is used.")
  expect_equal(music_time(z), "3/4")
  expect_warning(z <- c(as_music("a4", tempo = "4 = 100"), as_music("a4")),
                 "Tempo is inconsistent. Only the first is used.")
  expect_equal(music_tempo(z), "4 = 100")
  expect_equal(as.character(rep(as_music("a1 b2"), each = 2)), "a1 a1 b2 b2")
  expect_equal(as.character(rep(as_music("a1 b2"), times = 1:2)), "a1 b2 b2")

  expect_identical(rev(as_music("a1 b2")), as_music("b2 a1"))
  expect_equal(head(as_music("a8 b c d e f g")), as_music("a8 b c d e f"))
  expect_equal(tail(as_music("a8 b c d e f g")), as_music("b8 c d e f g"))

  y <- "a,4;5*5 b,4- c4 cgc'e'~4 cgc'e'1 e'4;2 c';3 g';4 c';5 ce';51"
  s <- as.character(c(rep(5, 7), 5432, 5432, 2, 3, 4, 5, 51))
  z <- as_music(y)

  expect_equal(music_strings(c(x, z)), c(rep("", length(x)), s))
  expect_equal(music_strings(c(z, x)), c(s, rep("", length(x))))
  expect_equal(music_strings(rep(z, 2)), rep(s, 2))
  expect_equal(music_strings(head(z, 2)), head(s, 2))
  expect_equal(music_strings(tail(z, 2)), tail(s, 2))
  expect_equal(music_strings(rev(z)), rev(s))
  expect_equal(music_strings(z[2:3]), s[2:3])
})

test_that("lyrics methods return as expected", {
  x <- as_lyrics(letters[1:12], format = "space")
  expect_equal(as.character(x[2:4]), "b c d")
  expect_equal(length(x[-1]), length(x) - 1)
  x[5:6] <- c("y", "8")
  expect_equal(x[5:6], as_lyrics("y 8"))
  x <- as_vector_time(x)
  expect_equal(as.character(x[x %in% c("a", "y", "8")]), c("a", "y","8"))

  x[[2]] <- "lyric"
  expect_equal(x[[2]], "lyric")

  x <- as_lyrics("x y z . . . apple")
  y <- as_lyrics(c("x", "y", "z", "", NA, ".", "apple"))
  expect_identical(x, as_space_time(y))
  expect_equal(length(x), 7)
  expect_equal(length(y), 7)

  expect_equal(as.character(c(x, y)), paste(x, x, sep = " "))
  expect_identical(c(x, as.character(x)), c(x, x))
  expect_identical(c(x, x), rep(x, 2))
  expect_equal(as.character(rep(as_lyrics("a b"), each = 2)), "a a b b")
  expect_equal(as.character(rep(as_lyrics("a b"), times = 1:2)), "a b b")

  expect_identical(rev(as_lyrics("a1 b2")), as_lyrics("b2 a1"))
  expect_equal(head(as_lyrics("a8 b c d e f g")), as_lyrics("a8 b c d e f"))
  expect_equal(tail(as_lyrics("a8 b c d e f g")), as_lyrics("b c d e f g"))
})

test_that("phrase methods return as expected", {
  x <- phrase("a b c'g'c''*2", "4", "5 5 543*2")
  x2 <- c(x, x)
  expect_equal(x2, rep(x, 2))
  expect_error(rep(x, each = 2), "Cannot use `each` with a phrase.")
  expect_error(rep(x, times = 1), "Cannot use `times` with a phrase.")
})
