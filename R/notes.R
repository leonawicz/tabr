#' Note and pitch helpers
#'
#' Helper functions for manipulating individual note and pitch strings.
#'
#' In this context, sharpening flats and flattening sharps refers to inverting their notation, not raising and lowering a flatted or sharped note by one semitone.
#' For the latter, use \code{naturalize}, which removes flat and/or sharp notation from a string.
#'
#' Due to its simplicity, for \code{note_rotate} the strings may include chords. It simply rotates anything space-delimited or vectorized in place.
#' Octave numbering is ignored if present.
#'
#' By contrast, for \code{note_shift} the entire sequence is shifted up or down, as if inverting a broken chord.
#' In this case \code{notes} is strictly interpreted and may not include chords.
#' Octave numbering applies, though large, multi-octave gaps will be condensed in the process.
#' Given the context of \code{note_shift}, the \code{notes} sequence should be ordered by increasing pitch.
#' If it is not, ordering will be forced with each inversion during the \code{n} shifts.
#'
#' \code{note_arpeggiate} also allows notes only. It is similar to \code{note_shift}, except that instead of a moving window,
#' it grows from the original set of notes by \code{n} in the direction of the sign of \code{n}.
#'
#' @param notes character, a noteworthy string, space-delimited or vector of individual entries.
#' @param type character, type of note to naturalize.
#' @param strip logical, strip any octave notation that may be present, returning only the basic notes without explicit pitch.
#' @param n integer, degree of rotation.
#' @param key character, key signature to coerce any accidentals to the appropriate form for the key. May also specify \code{"sharp"} or \code{"flat"}.
#' @param ... additional arguments to \code{transpose}, specifically \code{key} and \code{style}.
#'
#' @return character
#' @export
#' @name note-helpers
#'
#' @examples
#' x <- "a_ a a#"
#' note_is_natural(x)
#' note_is_accidental(x)
#' note_is_flat(x)
#' note_is_sharp(x)
#' note_set_key(x, "f")
#' note_set_key(x, "g")
#'
#' x <- "e_2 a_, c#f#a#"
#' naturalize(x)
#' naturalize(x, "sharp")
#' sharpen_flat(x)
#' flatten_sharp(x)
#'
#' note_rotate(x, 1)
#' note_shift("c e g", 1)
#' note_shift("c e g", -4)
#' note_arpeggiate("c e g", 5)
#' note_arpeggiate("c e g", -5)
note_is_natural <- function(notes){
  .check_note(notes)
  sapply(.uncollapse(notes), .pitch_natural, USE.NAMES = FALSE)
}

#' @export
#' @rdname note-helpers
note_is_accidental <- function(notes){
  .check_note(notes)
  sapply(.uncollapse(notes), .pitch_accidental, USE.NAMES = FALSE)
}

#' @export
#' @rdname note-helpers
note_is_flat <- function(notes){
  .check_note(notes)
  sapply(.uncollapse(notes), .pitch_flat, USE.NAMES = FALSE)
}

#' @export
#' @rdname note-helpers
note_is_sharp <- function(notes){
  .check_note(notes)
  sapply(.uncollapse(notes), .pitch_sharp, USE.NAMES = FALSE)
}

#' @export
#' @rdname note-helpers
naturalize <- function(notes, type = c("both", "flat", "sharp"), strip = FALSE){
  .check_noteworthy(notes)
  type <- match.arg(type)
  pat <- switch(type, both = "_|#", flat = "_", sharp = "#")
  x <- gsub(pat, "", notes)
  if(strip) x <- .pitch_to_note(x)
  x
}

#' @export
#' @rdname note-helpers
sharpen_flat <- function(notes, strip = FALSE){
  .check_noteworthy(notes)
  x <- if(length(notes) > 1) paste0(notes, collapse = " ") else notes
  x <- transpose(x, 0, key = "g", style = ifelse(strip, "strip", "default"))
  if(length(notes) > 1) x <- .uncollapse(x)
  x
}

#' @export
#' @rdname note-helpers
flatten_sharp <- function(notes, strip = FALSE){
  .check_noteworthy(notes)
  x <- if(length(notes) > 1) paste0(notes, collapse = " ") else notes
  x <- transpose(x, 0, key = "f", style = ifelse(strip, "strip", "default"))
  if(length(notes) > 1) x <- .uncollapse(x)
  x
}

#' @export
#' @rdname note-helpers
note_set_key <- function(notes, key = "c"){
  .check_noteworthy(notes)
  if(key == "flat") return(flatten_sharp(notes))
  if(key == "sharp") return(sharpen_flat(notes))
  .keycheck(key)
  if(key_is_natural(key)) return(notes)
  Recall(notes, .keydata$sf[.keydata$key == key])
}

#' @export
#' @rdname note-helpers
note_rotate <- function(notes, n = 0){
  .check_noteworthy(notes)
  x <- .uncollapse(notes)
  n <- n %% length(x)
  if(n == 0) return(notes)
  x <- x[c((n + 1):length(x), 1:n)]
  if(length(notes) == 1) x <- paste0(x, collapse = " ")
  x
}

#' @export
#' @rdname note-helpers
note_shift <- function(notes, n = 0){
  .check_note(notes)
  if(n == 0) return(notes)
  x <- .uncollapse(notes)
  nx <- length(x)
  style <- if(any(grepl(",|'", notes))) "tick" else "integer"
  if(nx == 1) return(transpose(x, 12 * n, style = style))
  idx <- (0:n)[-1]
  f <- function(x) if(x == 3) "" else x
  if(n > 0){
    for(i in idx){
      o <- .pitch_to_octave(x[nx])
      x[1] <- paste0(.pitch_to_note(x[1]), f(o))
      if(pitch_interval(x[1], x[nx]) > 0) x[1] <- paste0(.pitch_to_note(x[1]), f(o + 1))
      x <- x[c(2:(nx), 1)]
    }
  } else {
    for(i in idx){
      o <- .pitch_to_octave(x[1])
      x[nx] <- paste0(.pitch_to_note(x[nx]), f(o))
      if(pitch_interval(x[nx], x[1]) < 0) x[nx] <- paste0(.pitch_to_note(x[nx]), f(o - 1))
      x <- x[c(nx, 1:(nx - 1))]
    }
  }
  if(style == "tick") x <- .octavesub(x)
  if(length(notes) == 1) x <- paste0(x, collapse = " ")
  x
}

#' @export
#' @rdname note-helpers
note_arpeggiate <- function(notes, n = 0, ...){
  .check_note(notes)
  if(n == 0) return(notes)
  x <- .uncollapse(notes)
  sharp <- !grepl("_", paste(x, collapse = " ")) & grepl("#", paste(x, collapse = " "))
  if(!is.null(list(...)$key)){
    k <- .keydata[.keydata$key == list(...)$key, ]
    if(!is.na(k$sf)) sharp <- k$sf == "sharp"
  }
  nx <- length(x)
  style <- if(any(grepl(",|'", notes))) "tick" else "integer"
  if(nx == 1) return(transpose(x, 12 * n, style = style))
  s <- sign(n) * seq(12, 12 * (abs(n) %/% nx + abs(n) %% nx), by = 12)
  if(n > 0){
    x <- c(x, sapply(s, function(i) transpose(paste(x, collapse = " "), i, ...)))
    x <- paste(x, collapse = " ")
    x <- if(sharp) sharpen_flat(x) else flatten_sharp(x)
    x <- .uncollapse(x)[1:(nx + n)]
  } else {
    x <- c(sapply(rev(s), function(i) transpose(paste(x, collapse = " "), i, ...)), x)
    x <- paste(x, collapse = " ")
    x <- if(sharp) sharpen_flat(x) else flatten_sharp(x)
    x <- utils::tail(.uncollapse(x), nx - n)
  }
  if(style == "tick") x <- .octavesub(x)
  if(length(notes) == 1) x <- paste0(x, collapse = " ")
  x
}

#' Check note and chord validity
#'
#' Check whether a string is comprised exclusively of valid note and/or chord substring syntax.
#' \code{is_note} and \code{is_chord} are vectorized and their positive results are mutually exclusive.
#' \code{noteworthy} is also vectorized and performs both checks, but it returns a scalar logical result indicating whether the entire set contains exclusively valid entries.
#'
#' \code{is_diatonic} performs a vectorized logical check on a \code{noteworthy} string for all notes and chords.
#' TO check strictly notes or chords, see \code{\link{note_in_scale}} and \code{\link{chord_is_diatonic}}.
#'
#' @param x character, space-delimited entries or a vector of single, non-delimited entries.
#' @param key character, key signature.
#'
#' @return logical
#' @export
#' @name valid-notes
#' @seealso \code{\link{note_in_scale}}, \code{\link{chord_is_diatonic}}
#'
#' @examples
#' x <- "a# b_ c, d'' e3 g_4 A m c2e_2g2 cegh"
#' data.frame(
#'   x = strsplit(x, " ")[[1]],
#'   note = is_note(x),
#'   chord = is_chord(x),
#'   either = noteworthy(x))
#'
#' is_diatonic("ace ac#e d e_", "c")
is_note <- function(x){
  x <- .uncollapse(x)
  y1 <- grepl("[a-grs]", x) & !grepl("[h-qt-zA-Z]", x)
  y2 <- gsub("\\d|,|'|_|#|~|\\*", "", x)
  y1 & nchar(y2) == 1 & y2 == substr(x, 1, 1)
}

#' @export
#' @rdname valid-notes
is_chord <- function(x){
  x <- .uncollapse(x)
  y <- sapply(x, function(x) length(.split_chord(x)) > 1, USE.NAMES = FALSE)
  if(!any(y)) return(y)
  idx <- which(y)
  y[idx] <- sapply(x[idx], function(x) all(is_note(.split_chord(x))), USE.NAMES = FALSE)
  y
}

#' @export
#' @rdname valid-notes
noteworthy <- function(x) all(is_note(x) | is_chord(x))

#' @export
#' @rdname valid-notes
is_diatonic <- function(x, key = "c"){
  .check_noteworthy(x)
  s <- scale_diatonic(key)
  x <- .uncollapse(x)
  sapply(x, function(x) all(.pitch_to_note(.split_chord(x)) %in% s), USE.NAMES = FALSE)
}

.check_note <- function(x) if(any(!is_note(x))) stop("Invalid note found.", call. = FALSE)

.check_chord <- function(x) if(any(!is_chord(x))) stop("Invalid chord found.", call. = FALSE)

.check_noteworthy <- function(x) if(!noteworthy(x)) stop("Invalid notes or chords found.", call. = FALSE)

.uncollapse <- function(x){
  if(length(x) == 1) x <- strsplit(x, " ")[[1]]
  f <- function(x){
    if(!grepl("\\*\\d+", x)) return(x)
    x <- strsplit(x, "\\*")[[1]]
    rep(x[1], as.integer(x[2]))
  }
  unlist(lapply(x, f))
}

#' Note, pitch and chord equivalence
#'
#' Helper functions to check the equivalence of two noteworthy strings, and other related functions.
#'
#' Noteworthy strings may contain notes, pitches and chords. Noteworthy strings are equal if they sound the same.
#' This means that if one string contains Eb (\code{e_}) and the other contains D# (\code{d#}) then the two strings may be equal, but they are not identical.
#'
#' \code{pitch_is_equal} and \code{pitch_is_identical} perform these respective tests of equivalence on both notes and chords.
#' These are the strictest functions in terms of equivalent sound because pitch includes the octave number.
#'
#' \code{note_is_equal} and \code{note_is_identical} are similar but include a default argument \code{ignore_octave = TRUE}, focusing only on the notes and chords.
#' This allows an even more relaxed definition of equivalence. Setting this argument to \code{FALSE} is the same as calling the \code{pitch_is_*} variant.
#'
#' Chords can be checked the same as notes. Every note in the sequence is checked pairwise between \code{note1} and \code{note2}.
#'
#' These functions will return \code{TRUE} or \code{FALSE} for every timestep in a sequence.
#' If the two noteworthy strings do not contain the same number of notes at a specific step, such as a single note compared to a chord, this yields a \code{FALSE} value,
#' even in a case of an octave dyad with octave number ignored.
#' If the two sequences have unequal length an error is thrown.
#' These are bare minimum requirement for equivalence. See examples.
#'
#' \code{octave_is_equal} and \code{octave_is_identical} allow much weaker forms of equivalence in that they ignore notes completely.
#' These functions are only concerned with comparing the octave numbers spanned by any pitches present at each timestep.
#' \code{octave_is_equal} only looks at the octave number associated with the first not at each step, so only the root note of a chord, when checking for equality.
#' \code{octave_is_identical} compares all octaves spanned at a given timestep. It does not matter if comparing two chords containing different numbers of notes.
#' If the set of octaves spanned by one chord is identical to the set spanned by the other, they are considered to have identical octave coverage.
#' For example, \code{a1b2c3} is identical to \code{d1e1f2g3}. To be equal, it only matters that the two chords begin with \code{x1} where \code{x} is any note.
#' Alternatively, for \code{octave_is_identical} only, setting \code{single_octave = TRUE} additionally requires that all notes from both chords being compared at a given timestep share a single octave.
#'
#' @param notes1 character, note string, space-delimited or vector of individual entries.
#' @param notes2 character, note string, space-delimited or vector of individual entries.
#' @param ignore_octave logical, ignore octave position when considering equivalence.
#' @param single_octave logical, for octave equality, require all notes share the same octave. See details.
#'
#' @return logical
#' @export
#' @name note-equivalence
#'
#' @examples
#' x <- "b_2 ce_g"
#' y <- "b_ cd#g"
#' note_is_equal(x, y)
#' note_is_identical(x, y)
#'
#' x <- "b_2 ce_g"
#' y <- "b_2 cd#g"
#' pitch_is_equal(x, y)
#' pitch_is_identical(x, y)
#'
#' # same number of same notes, same order: unequal sequence length
#' x <- "b_2 ce_g b_"
#' y <- "b_2 ce_gb_"
#' note_is_equal(x, y)
#'
#' # same number of same notes, same order, equal length: unequal number per timestep
#' x <- "b_2 ce_g b_"
#' y <- "b_2 ce_ gb_"
#' note_is_equal(x, y)
#'
#' x <- "a1 b_2 a1b2c3 a1b4 g1a1b1"
#' y <- "a_2 g#2 d1e1f2g3 a1b2b4 d1e1"
#' octave_is_equal(x, y)
#' octave_is_identical(x, y)
#' octave_is_identical(x, y, single_octave = TRUE)
note_is_equal <- function(notes1, notes2, ignore_octave = TRUE){
  x <- .check_comparison(notes1, notes2)
  if(any(is.na(x))) return(NA)
  if(ignore_octave){
    x[[1]] <- .pitch_to_note(x[[1]])
    x[[2]] <- .pitch_to_note(x[[2]])
  }
  sapply(seq_along(x[[1]]), function(i){
    length(.split_chord(x[[1]][i])) == length(.split_chord(x[[1]][i])) &
      flatten_sharp(x[[1]][i]) == flatten_sharp(x[[2]][i])
  })
}

#' @export
#' @rdname note-equivalence
note_is_identical <- function(notes1, notes2, ignore_octave = TRUE){
  x <- .check_comparison(notes1, notes2)
  if(any(is.na(x))) return(NA)
  if(ignore_octave){
    x[[1]] <- .pitch_to_note(x[[1]])
    x[[2]] <- .pitch_to_note(x[[2]])
  }
  y <- sapply(seq_along(x[[1]]), function(i){
    length(.split_chord(x[[1]][i])) == length(.split_chord(x[[1]][i]))
  })
  x[[1]] == x[[2]] & y
}

#' @export
#' @rdname note-equivalence
pitch_is_equal <- function(notes1, notes2){
  x <- .check_comparison(notes1, notes2)
  if(any(is.na(x))) return(NA)
  sapply(seq_along(x[[1]]), function(i){
    length(.split_chord(x[[1]][i])) == length(.split_chord(x[[1]][i])) &
      flatten_sharp(x[[1]][i]) == flatten_sharp(x[[2]][i])
  })
}

#' @export
#' @rdname note-equivalence
pitch_is_identical <- function(notes1, notes2){
  x <- .check_comparison(notes1, notes2)
  if(any(is.na(x))) return(NA)
  y <- sapply(seq_along(x[[1]]), function(i){
    length(.split_chord(x[[1]][i])) == length(.split_chord(x[[1]][i]))
  })
  x[[1]] == x[[2]] & y
}

#' @export
#' @rdname note-equivalence
octave_is_equal <- function(notes1, notes2){
  x <- .check_comparison(notes1, notes2)
  if(any(is.na(x))) return(NA)
  sapply(seq_along(x[[1]]), function(i){
    o1 <- as.integer(sapply(.split_chord(x[[1]][i]), .pitch_to_octave))[1]
    o2 <- as.integer(sapply(.split_chord(x[[2]][i]), .pitch_to_octave))[1]
    o1 == o2
  })
}

#' @export
#' @rdname note-equivalence
octave_is_identical <- function(notes1, notes2, single_octave = FALSE){
  x <- .check_comparison(notes1, notes2)
  if(any(is.na(x))) return(NA)
  sapply(seq_along(x[[1]]), function(i){
    o1 <- as.integer(sapply(.split_chord(x[[1]][i]), .pitch_to_octave))
    if(single_octave && length(o1) > 1 && !all(o1 == o1[1])) return(FALSE)
    o2 <- as.integer(sapply(.split_chord(x[[2]][i]), .pitch_to_octave))
    if(single_octave && length(o2) > 1 && !all(o2 == o2[1])) return(FALSE)
    if(any(!o1 %in% o2) | any(!o2 %in% o1)) return(FALSE)
    TRUE
  })
}

.check_comparison <- function(x1, x2){
  .check_noteworthy(x1)
  .check_noteworthy(x2)
  x1 <- .uncollapse(x1)
  x2 <- .uncollapse(x2)
  if(length(x1) != length(x2)) return(NA)
  list(x1, x2)
}

#' @export
#' @rdname note-equivalence
