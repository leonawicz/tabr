#' Chord inversion
#'
#' This function inverts a single chord given as a character string.
#' If \code{n = 0}, \code{chord} is returned immediately.
#' Otherwise, the notes of the chord are inverted. If \code{abs(n)} is greater
#' than the number of inversions (excluding root position), an error is thrown.
#'
#' Note that \code{chord_invert} has no knowledge of whether a chord might be
#' considered as in root position or some inversion already, as informed by a
#' key signature, chord name or user's intent.
#' This function simply inverts what it receives, treating any defined chord
#' string as in root position.
#'
#' Octave number applies to this function. Chords should always be defined by
#' notes of increasing pitch. Remember that an unspecified octave number on a
#' note is octave 3.
#' When the chord is inverted, it moves up the scale.
#' The lowest note is moved to the top of the chord, increasing its octave if
#' necessary, to ensure that the note takes the lowest octave number while
#' having the highest pitch.
#' The second lowest note becomes the lowest. It's octave does not change.
#' This pattern is repeated for higher order inversions. The opposite happens
#' if \code{n} is negative.
#'
#' The procedure ensures that the resulting inverted chord is still defined by
#' notes of increasing pitch.
#' However, if you construct an unusual chord that spans multiple octaves, the
#' extra space will be condensed by inversion.
#'
#' @param chord character, a single chord.
#' @param n inversion.
#' @param limit logical, limit inversions in either direction to one less than
#' the number of notes in the chord.
#'
#' @return character
#' @export
#'
#' @examples
#' chord_invert("ce_gb_", 3)
chord_invert <- function(chord, n = 0, limit = FALSE){
  .check_chord(chord)
  if(grepl(" ", chord))
    stop("`x` must be a single chord, not space-delimited chords.",
         call. = FALSE)
  if(n == 0) return(chord)
  x <- .split_chord(chord)
  nx <- length(x)
  if(abs(n) > nx - 1 & limit == TRUE)
    stop(paste0("Chord has ", nx, " notes. `n` must be in -", nx - 1, ":",
                nx - 1, ". Set `limit = FALSE` to override."), call. = FALSE)
  idx <- (0:n)[-1]
  f <- function(x) if(x == 3) "" else x
  if(n > 0){
    for(i in idx){
      o <- .pitch_to_octave(x[nx])
      x[1] <- paste0(.pitch_to_note(x[1]), f(o))
      if(pitch_interval(x[1], x[nx]) > 0)
        x[1] <- paste0(.pitch_to_note(x[1]), f(o + 1))
      x <- x[c(2:nx, 1)]
    }
  } else {
    for(i in idx){
      o <- .pitch_to_octave(x[1])
      x[nx] <- paste0(.pitch_to_note(x[nx]), f(o))
      if(pitch_interval(x[nx], x[1]) < 0)
        x[nx] <- paste0(.pitch_to_note(x[nx]), f(o - 1))
      x <- x[c(nx, 1:(nx - 1))]
    }
  }
  x <- paste(x, collapse = "")
  if(grepl(",|'", x)) x <- .octave_to_tick(x)
  .asnw(x)
}

#' Arpeggiate a chord
#'
#' Create an arpeggio from a chord.
#'
#' This function is based on \code{chord_invert}. If \code{n = 0} then
#' \code{chord} is returned immediately; other arguments are ignored.
#'
#' @param chord character, a single chord.
#' @param n integer, number of steps, negative indicates reverse direction
#' (decreasing pitch).
#' @param by whether each of the \code{n} steps refers to individual notes in
#' the chord (an inversion) or raising the entire chord in its given position
#' by one octave.
#' @param broken logical, return result as an arpeggio of broken chords.
#' @param collapse logical, collapse result into a single string ready for
#' phrase construction.
#'
#' @return character
#' @export
#'
#' @examples
#' chord_arpeggiate("ce_gb_", 2)
#' chord_arpeggiate("ce_gb_", -2)
#' chord_arpeggiate("ce_gb_", 2, by = "chord")
#' chord_arpeggiate("ce_gb_", 1, broken = TRUE, collapse = TRUE)
chord_arpeggiate <- function(chord, n = 0, by = c("note", "chord"),
                             broken = FALSE, collapse = FALSE){
  chord <- .uncollapse(chord)
  if(length(chord) != 1) stop("`chord` must be a single chord.", call. = FALSE)
  .check_chord(chord)
  if(n == 0) return(.asnw(chord))
  by <- match.arg(by)
  if(by == "note"){
    s <- 1:abs(n)
  } else {
    nc <- length(.split_chord(chord))
    s <- seq(nc, nc * abs(n), by = nc)
  }
  x <- c(chord, sapply(sign(n) * s, function(i) chord_invert(chord, i)))
  if(broken) x <- unlist(lapply(x, .split_chord))
  if(any(grepl(",|'", x))) x <- .octave_to_tick(x)
  if(collapse) x <- paste0(x, collapse = " ")
  .asnw(x)
}

#' Broken chords
#'
#' Convert chords in a noteworthy string or vector to broken chords.
#'
#' @param notes character, noteworthy string that may contain chords.
#'
#' @return character
#' @export
#'
#' @examples
#' chord_break("c e g ceg ceg")
chord_break <- function(notes){
  .check_noteworthy(notes)
  x <- .uncollapse(notes)
  x <- sapply(x, function(x) paste0(.split_chord(x), collapse = " "),
              USE.NAMES = FALSE)
  if(length(notes) == 1) x <- paste0(x, collapse = " ")
  .asnw(x)
}

#' Construct a dyad
#'
#' Construct a dyad given one note, an interval, and a direction.
#'
#' The \code{interval} may be specified by semitones of by common interval name
#' or abbreviation. See examples.
#' For a complete list of valid interval names and abbreviations see
#' \code{\link{mainIntervals}}.
#' \code{key} enforces the use of sharps or flats. This function is based on
#' \code{transpose}.
#' \code{notes} and \code{interval} may be vectors, but must be equal length.
#' Recycling occurs only if one argument is scalar.
#'
#' @param notes character, a noteworthy string, single notes only, no chords.
#' Number of timesteps must equal the length of \code{interval}.
#' @param interval integer or character vector; semitones or interval ID,
#' respectively. See details.
#' @param reverse logical, reverse the transposition direction. Useful when
#' \code{interval} is character.
#' @param octaves,accidentals,key See \code{\link{transpose}}.
#'
#' @return character
#' @export
#' @seealso \code{\link{mainIntervals}}
#'
#' @examples
#' dyad("a", 4)
#' x <- c("minor third", "m3", "augmented second", "A2")
#' dyad("a", x)
#' dyad("c'", x, reverse = TRUE)
#'
#' x <- c("M3", "m3", "m3", "M3", "M3", "m3", "m3")
#' dyad(letters[c(3:7, 1, 2)], x)
#'
#' x <- c("P1", "m3", "M3", "P4", "P5", "P8", "M9")
#' dyad("c", x)
#' dyad("c", x, reverse = TRUE)
#' dyad("d e", "m3")
dyad <- function(notes, interval, reverse = FALSE,
                 octaves = c("tick", "integer"),
                 accidentals = c("flat", "sharp"), key = NULL){
  if(is.character(interval)) interval <- interval_semitones(interval)
  if(any(is.na(interval))) stop("Invalid `interval`.", call. = FALSE)
  .check_note(notes)
  x <- .uncollapse(notes)
  o <- match.arg(octaves)
  a <- match.arg(accidentals)
  if(reverse) interval <- -interval
  nn <- length(x)
  ni <- length(interval)
  if(nn != ni){
    if(nn != 1 & ni != 1)
      stop("`notes` and `interval` have unequal lengths both > 1.",
           call. = FALSE)
    if(nn == 1) x <- rep(x, ni)
    if(ni == 1) interval <- rep(interval, nn)
  }
  f <- function(i, n1, int){
    n1 <- n1[i]
    int <- int[i]
    n2 <- transpose(n1, int, o, a, key)
    paste(if(int == 0) n1 else if(int > 0) paste0(n1, n2) else paste0(n2, n1),
          collapse = "")
  }
  x <- sapply(seq_along(x), f, x, interval)
  if(length(as.character(notes)) == 1) x <- paste(x, collapse = " ")
  .asnw(x)
}

#' Rank, order and sort chords and notes
#'
#' Rank, order and sort chords and notes by various definitions.
#'
#' There are three options for comparing the relative pitch position of chords
#' provided: comparison of the lowest or root note of each chord, the highest
#' pitch note, or taking the mean of all notes in a chord.
#'
#' @param notes character, a noteworthy string.
#' @param pitch character, how ranking of chords is determined; lowest pitch,
#' mean pitch, or highest pitch.
#' @param decreasing logical, sort in decreasing order.
#' @param ... additional arguments passed to \code{rank} or \code{order}.
#'
#' @return integer for rank and order, character for sort
#' @export
#' @name chord-compare
#'
#' @examples
#' x <- "a2 c a2 ceg ce_g cea"
#' chord_rank(x, "min")
#' chord_rank(x, "max")
#' chord_rank(x, "mean")
#'
#' chord_order(x)
#' chord_order(x, "mean")
#' chord_sort(x, "mean")
chord_rank <- function(notes, pitch = c("min", "mean", "max"), ...){
  rank(.chord_rank(notes, pitch), ...)
}

#' @export
#' @rdname chord-compare
chord_order <- function(notes, pitch = c("min", "mean", "max"), ...){
  order(.chord_rank(notes, pitch), ...)
}

#' @export
#' @rdname chord-compare
chord_sort <- function(notes, pitch = c("min", "mean", "max"),
                       decreasing = FALSE, ...){
  ord <- chord_order(notes, pitch, ...)
  x <- .uncollapse(notes)[ord]
  if(decreasing) x <- rev(x)
  if(length(notes) == 1) x <- paste0(x, collapse = " ")
  .asnw(x)
}

.chord_rank <- function(x, pitch = c("min", "mean", "max")){
  .check_noteworthy(x)
  pitch <- match.arg(pitch)
  x <- .uncollapse(x)
  sapply(x, function(y){
    y <- sapply(.split_chord(y), function(i) pitch_interval("c", i))
    switch(pitch, "min" = min(y), "mean" = mean(y), "max" = max(y))
  }, USE.NAMES = FALSE)
}

#' Extract notes from chords
#'
#' Filter or slice chords to extract individual notes.
#'
#' These functions extract notes from chords such as the root note, the highest
#' pitch, specific position among the notes by pitch, or trim chords to
#' simplify them.
#' They operate based only on ordered pitches.
#'
#' For \code{chord_slice}, any entry that is empty after slicing is dropped.
#' An error is thrown is \code{index} is completely out of bounds for all
#' chords.
#'
#' @param notes character, a noteworthy string.
#' @param index integer, the order of a note in a chord by pitch (not scale
#' degrees).
#'
#' @return a noteworthy string
#' @export
#' @name chord-filter
#'
#' @examples
#' x <- "a_2 c#eg# e_gc egc,cc'"
#' chord_root(x)
#' chord_top(x)
#' identical(chord_slice(x, 1), chord_root(x))
#' chord_slice(x, 2)
#' chord_slice(x, 4)
#' chord_slice(x, 3:5)
chord_root <- function(notes){
  .check_noteworthy(notes)
  z <- .infer_types(notes)
  x <- .chord_root(.uncollapse(notes), z$o, z$a)
  if(length(notes) == 1) x <- paste0(x, collapse = " ")
  .asnw(x)
}

#' @export
#' @rdname chord-filter
chord_top <- function(notes){
  .check_noteworthy(notes)
  z <- .infer_types(notes)
  x <- .chord_top(.uncollapse(notes), z$o, z$a)
  if(length(notes) == 1) x <- paste0(x, collapse = " ")
  .asnw(x)
}

#' @export
#' @rdname chord-filter
chord_slice <- function(notes, index){
  .check_noteworthy(notes)
  x <- .uncollapse(notes)
  x <- sapply(x, function(y){
    y <- .split_chord(y)
    ord <- order(sapply(y, function(i) pitch_interval("c", i)))
    y <- y[ord][index]
    y <- y[!is.na(y)]
    if(length(y)) paste(y, collapse = "") else NA_character_
  }, USE.NAMES = FALSE)
  x <- x[!is.na(x)]
  if(!length(x)) stop("Index out of bounds for all chords.", call. = FALSE)
  if(length(notes) == 1) x <- paste0(x, collapse = " ")
  .asnw(x)
}

.chord_root <- function(x, octaves = "tick", accidentals = "flat"){
  purrr::map_chr(x, ~.pitch_min(.split_chords(.x), octaves, accidentals))
}

.chord_top <- function(x, octaves = "tick", accidentals = "flat"){
  purrr::map_chr(x, ~.pitch_max(.split_chords(.x), octaves, accidentals))
}

#' Check if chords are major or minor
#'
#' Check if chords are major or minor where possible.
#'
#' These functions operate based only on ordered pitches.
#' They do not recognize what a human might interpret and name an inverted
#' chord with a root other than the lowest pitch.
#' This imposes limitations on the utility of these functions,
#' which scan the intervals for a minor or major third in a chord whose notes
#' are sorted by pitch.
#'
#' In several cases including single notes or no major or minor third interval
#' present, \code{NA} is returned.
#' \code{TRUE} or \code{FALSE} is only returned if such an interval is present.
#' If more than one is present, it is based on the lowest in pitch.
#' It prioritizes major/minor and minor/major adjacent intervals (recognizing a
#' common triad). If these do not occur adjacent, the lowest third is selected.
#' This is still imperfect, but a useful method. Second and higher unknown
#' chord inversions are problematic.
#'
#' @param notes character, a noteworthy string.
#'
#' @return logical vector
#' @export
#'
#' @examples
#' x <- "c cg, ce ce_ ceg ce_gb g,ce g,ce_ e_,g,c e_,g,ce_ e_,g,c"
#' chord_is_major(x)
#' identical(chord_is_major(x), !chord_is_minor(x))
chord_is_major <- function(notes){
  .check_noteworthy(notes)
  x <- .uncollapse(notes)
  sapply(x, .chord_is_major, USE.NAMES = FALSE)
}

#' @export
#' @rdname chord_is_major
chord_is_minor <- function(notes){
  !chord_is_major(notes)
}

.chord_is_major <- function(x){
  x <- .split_chord(x)
  f <- function(i) pitch_interval("c", i)
  int <- diff(sapply(x, f))
  if(!length(int)) return(NA)
  if(length(int) == 1){
    if(int == 3) return(FALSE)
    if(int == 4) return(TRUE)
    return(NA)
  }
  if(!length(intersect(3:4, int))) return(NA)
  if(3 %in% int & !4 %in% int) return(FALSE)
  if(4 %in% int & !3 %in% int) return(TRUE)
  idx3 <- which(int == 3)
  idx4 <- which(int == 4)
  if(any((idx3 - 1) %in% idx4)){
    major <- (idx3 - 1)[(idx3 - 1) %in% idx4][1]
  } else {
    major <- NA
  }
  if(any((idx4 - 1) %in% idx3)){
    minor <- (idx4 - 1)[(idx4 - 1) %in% idx3][1]
  } else {
    minor <- NA
  }
  if(!is.na(major) & !is.na(minor)){
    if(major < minor) return(TRUE) else return(FALSE)
  }
  if(is.na(minor) & !is.na(major)) return(TRUE)
  if(is.na(major) & !is.na(minor)) return(FALSE)
  if(min(idx4) < min(idx3)) return(TRUE) else return(FALSE)
}

#' Chord constructors
#'
#' These functions construct basic chord string notation from root \code{notes}.
#'
#' Providing a \code{key} signature is used only to ensure flats or sharps for
#' accidentals.
#' An additional set of aliases with efficient names, of the
#' form \code{x*} where \code{*} is a chord modifier abbreviation, is provided
#' to complement the set of \code{chord_*} functions.
#'
#' These functions create standard chords, not the multi-octave spanning types
#' of chords commonly played on guitar.
#'
#' @param notes character, a noteworthy string of chord root notes.
#' @param key key signature. See details.
#' @param octaves character, passed to \code{transpose}.
#'
#' @return character
#' @export
#' @name chords
#' @seealso \code{\link{transpose}}
#'
#' @examples
#' chord_min("d")
#' chord_maj("d")
#' xM("d")
#' xm("c f g")
#' xm("c, f, g,", key = "e_")
chord_min <- function(notes, key = "c", octaves = "tick"){
  semitones <- c(0, 3, 7)
  .chord_prep(notes, semitones, key, octaves)
}

#' @export
#' @rdname chords
chord_maj <- function(notes, key = "c", octaves = "tick"){
  semitones <- c(0, 4, 7)
  .chord_prep(notes, semitones, key, octaves)
}

#' @export
#' @rdname chords
chord_min7 <- function(notes, key = "c", octaves = "tick"){
  semitones <- c(0, 3, 7, 10)
  .chord_prep(notes, semitones, key, octaves)
}

#' @export
#' @rdname chords
chord_dom7 <- function(notes, key = "c", octaves = "tick"){
  semitones <- c(0, 4, 7, 10)
  .chord_prep(notes, semitones, key, octaves)
}

#' @export
#' @rdname chords
chord_7s5 <- function(notes, key = "c", octaves = "tick"){
  semitones <- c(0, 4, 8, 10)
  .chord_prep(notes, semitones, key, octaves)
}

#' @export
#' @rdname chords
chord_maj7 <- function(notes, key = "c", octaves = "tick"){
  semitones <- c(0, 4, 7, 11)
  .chord_prep(notes, semitones, key, octaves)
}

#' @export
#' @rdname chords
chord_min6 <- function(notes, key = "c", octaves = "tick"){
  semitones <- c(0, 3, 7, 9)
  .chord_prep(notes, semitones, key, octaves)
}

#' @export
#' @rdname chords
chord_maj6 <- function(notes, key = "c", octaves = "tick"){
  semitones <- c(0, 4, 7, 9)
  .chord_prep(notes, semitones, key, octaves)
}

#' @export
#' @rdname chords
chord_dim <- function(notes, key = "c", octaves = "tick"){
  semitones <- c(0, 3, 6)
  .chord_prep(notes, semitones, key, octaves)
}

#' @export
#' @rdname chords
chord_dim7 <- function(notes, key = "c", octaves = "tick"){
  semitones <- c(0, 3, 6, 9)
  .chord_prep(notes, semitones, key, octaves)
}

#' @export
#' @rdname chords
chord_m7b5 <- function(notes, key = "c", octaves = "tick"){
  semitones <- c(0, 3, 6, 10)
  .chord_prep(notes, semitones, key, octaves)
}

#' @export
#' @rdname chords
chord_aug <- function(notes, key = "c", octaves = "tick"){
  semitones <- c(0, 4, 8)
  .chord_prep(notes, semitones, key, octaves)
}

#' @export
#' @rdname chords
chord_5 <- function(notes, key = "c", octaves = "tick"){
  semitones <- c(0, 7)
  .chord_prep(notes, semitones, key, octaves)
}

#' @export
#' @rdname chords
chord_sus2 <- function(notes, key = "c", octaves = "tick"){
  semitones <- c(0, 2, 7)
  .chord_prep(notes, semitones, key, octaves)
}

#' @export
#' @rdname chords
chord_sus4 <- function(notes, key = "c", octaves = "tick"){
  semitones <- c(0, 5, 7)
  .chord_prep(notes, semitones, key, octaves)
}

#' @export
#' @rdname chords
chord_dom9 <- function(notes, key = "c", octaves = "tick"){
  semitones <- c(0, 4, 7, 10, 14)
  .chord_prep(notes, semitones, key, octaves)
}

#' @export
#' @rdname chords
chord_7s9 <- function(notes, key = "c", octaves = "tick"){
  semitones <- c(0, 4, 7, 10, 15)
  .chord_prep(notes, semitones, key, octaves)
}

#' @export
#' @rdname chords
chord_maj9 <- function(notes, key = "c", octaves = "tick"){
  semitones <- c(0, 4, 7, 11, 14)
  .chord_prep(notes, semitones, key, octaves)
}

#' @export
#' @rdname chords
chord_add9 <- function(notes, key = "c", octaves = "tick"){
  semitones <- c(0, 4, 7, 14)
  .chord_prep(notes, semitones, key, octaves)
}

#' @export
#' @rdname chords
chord_min9 <- function(notes, key = "c", octaves = "tick"){
  semitones <- c(0, 3, 7, 10, 14)
  .chord_prep(notes, semitones, key, octaves)
}

#' @export
#' @rdname chords
chord_madd9 <- function(notes, key = "c", octaves = "tick"){
  semitones <- c(0, 3, 7, 14)
  .chord_prep(notes, semitones, key, octaves)
}

#' @export
#' @rdname chords
chord_min11 <- function(notes, key = "c", octaves = "tick"){
  semitones <- c(0, 3, 7, 10, 14, 17)
  .chord_prep(notes, semitones, key, octaves)
}

#' @export
#' @rdname chords
chord_7s11 <- function(notes, key = "c", octaves = "tick"){
  semitones <- c(0, 4, 7, 10, 18)
  .chord_prep(notes, semitones, key, octaves)
}

#' @export
#' @rdname chords
chord_maj7s11 <- function(notes, key = "c", octaves = "tick"){
  semitones <- c(0, 4, 7, 11, 14, 18)
  .chord_prep(notes, semitones, key, octaves)
}

#' @export
#' @rdname chords
chord_11 <- function(notes, key = "c", octaves = "tick"){
  semitones <- c(0, 7, 10, 14, 17)
  .chord_prep(notes, semitones, key, octaves)
}

#' @export
#' @rdname chords
chord_maj11 <- function(notes, key = "c", octaves = "tick"){
  semitones <- c(0, 4, 7, 11, 14, 17)
  .chord_prep(notes, semitones, key, octaves)
}

#' @export
#' @rdname chords
chord_13 <- function(notes, key = "c", octaves = "tick"){
  semitones <- c(0, 4, 7, 10, 14, 21)
  .chord_prep(notes, semitones, key, octaves)
}

#' @export
#' @rdname chords
chord_min13 <- function(notes, key = "c", octaves = "tick"){
  semitones <- c(0, 3, 7, 10, 14, 21)
  .chord_prep(notes, semitones, key, octaves)
}

#' @export
#' @rdname chords
chord_maj13 <- function(notes, key = "c", octaves = "tick"){
  semitones <- c(0, 4, 7, 11, 14, 17, 21)
  .chord_prep(notes, semitones, key, octaves)
}

.chord_prep <- function(notes, semitones, key, octaves){
  .check_note(notes)
  .keycheck(key)
  x <- sapply(.uncollapse(notes), function(note){
    paste0(
      sapply(semitones,
             function(s) transpose(note, s, key = key, octaves = octaves)),
      collapse = "")
  }, USE.NAMES = FALSE)
  if(length(notes) == 1) x <- paste0(x, collapse = " ")
  .asnw(x)
}

# lintr will catch function name casing violation # nolint start

#' @export
#' @rdname chords
xm <- chord_min

#' @export
#' @rdname chords
xM <- chord_maj

#' @export
#' @rdname chords
xm7 <- chord_min7

#' @export
#' @rdname chords
x7 <- chord_dom7

#' @export
#' @rdname chords
x7s5 <- chord_7s5

#' @export
#' @rdname chords
xM7 <- chord_maj7

#' @export
#' @rdname chords
xm6 <- chord_min6

#' @export
#' @rdname chords
xM6 <- chord_maj6

#' @export
#' @rdname chords
xdim <- chord_dim

#' @export
#' @rdname chords
xdim7 <- chord_dim7

#' @export
#' @rdname chords
xm7b5 <- chord_m7b5

#' @export
#' @rdname chords
xaug <- chord_aug

#' @export
#' @rdname chords
x5 <- chord_5

#' @export
#' @rdname chords
xs2 <- chord_sus2

#' @export
#' @rdname chords
xs4 <- chord_sus4

#' @export
#' @rdname chords
x9 <- chord_dom9

#' @export
#' @rdname chords
x7s9 <- chord_7s9

#' @export
#' @rdname chords
xM9 <- chord_maj9

#' @export
#' @rdname chords
xadd9 <- chord_add9

#' @export
#' @rdname chords
xm9 <- chord_min9

#' @export
#' @rdname chords
xma9 <- chord_madd9

#' @export
#' @rdname chords
xm11 <- chord_min11

#' @export
#' @rdname chords
x7s11 <- chord_7s11

#' @export
#' @rdname chords
xM7s11 <- chord_maj7s11

#' @export
#' @rdname chords
x_11 <- chord_11

#' @export
#' @rdname chords
xM11 <- chord_maj11

#' @export
#' @rdname chords
x_13 <- chord_13

#' @export
#' @rdname chords
xm13 <- chord_min13

#' @export
#' @rdname chords
xM13 <- chord_maj13

# nolint end
