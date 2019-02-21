#' Chord inversion
#'
#' This function inverts a single chord given as a character string. If \code{n = 0}, \code{chord} is returned immediately.
#' Otherwise, the notes of the chord are inverted. If \code{abs(n)} is greater than the number of inversions (excluding root position), an error is thrown.
#'
#' Note that \code{chord_invert} has no knowledge of whether a chord might be considered as in root position or some inversion already, as informed by a key signature, chord name or user's intent.
#' This function simply inverts what it receives, treating any defined chord string as in root position.
#'
#' Octave number applies to this function. Chords should always be defined by notes of increasing pitch. Remember that an unspecified octave number on a note is octave 3.
#' When the chord is inverted, it moves up the scale.
#' The lowest note is moved to the top of the chord, increasing its octave if necessary, to ensure that the note takes the lowest octave number while having the highest pitch.
#' The second lowest note becomes the lowest. It's octave does not change.
#' This pattern is repeated for higher order inversions. The opposite happens if \code{n} is negative.
#'
#' The procedure ensures that the resulting inverted chord is still defined by notes of increasing pitch.
#' However, if you construct an unusual chord that spans multiple octaves, the extra space will be condensed by inversion.
#'
#' @param chord character, a single chord.
#' @param n inversion.
#' @param limit logical, limit inversions in either direction to one less than the number of notes in the chord.
#'
#' @return character
#' @export
#'
#' @examples
#' chord_invert("ce_gb_", 3)
chord_invert <- function(chord, n = 0, limit = FALSE){
  .check_chord(chord)
  if(grepl(" ", chord)) stop("`x` must be a single chord, not space-delimited chords.", call. = FALSE)
  if(n == 0) return(chord)
  x <- .split_chord(chord)
  nx <- length(x)
  if(nx == 1) return(x)
  if(abs(n) > nx - 1 & limit == TRUE)
    stop(paste0("Chord has ", nx, " notes. `n` must be in -", nx - 1, ":", nx - 1,
                ". Set `limit = FALSE` to override."), call. = FALSE)
  idx <- (0:n)[-1]
  f <- function(x) if(x == 3) "" else x
  if(n > 0){
    for(i in idx){
      o <- .pitch_to_octave(x[nx])
      x[1] <- paste0(.pitch_to_note(x[1]), f(o))
      if(pitch_interval(x[1], x[nx]) > 0) x[1] <- paste0(.pitch_to_note(x[1]), f(o + 1))
      x <- x[c(2:nx, 1)]
    }
  } else {
    for(i in idx){
      o <- .pitch_to_octave(x[1])
      x[nx] <- paste0(.pitch_to_note(x[nx]), f(o))
      if(pitch_interval(x[nx], x[1]) < 0) x[nx] <- paste0(.pitch_to_note(x[nx]), f(o - 1))
      x <- x[c(nx, 1:(nx - 1))]
    }
  }
  paste(x, collapse = "")
}

#' Broken chords
#'
#' Convert chords in a noteworthy string or vector to broken chords.
#'
#' @param notes character, note string that may contain chords.
#'
#' @return character
#' @export
#'
#' @examples
#' chord_break("c e g ceg ceg")
chord_break <- function(notes){
  .check_noteworthy(notes)
  x <- .uncollapse(notes)
  x <- sapply(x, function(x) paste0(.split_chord(x), collapse = " "), USE.NAMES = FALSE)
  if(length(notes) == 1) x <- paste0(x, collapse = " ")
  x
}

#' Check if a chord is diatonic
#'
#' Check whether a chord is diatonic in a given key.
#'
#' This function strictly accepts chord strings. To check if notes are in a scale, see \code{\link{note_in_scale}}. To check generally if a \code{noteworthy} string is fully diatonic, see \code{\link{is_diatonic}}.
#'
#' @param chord character, a chord string. May be a vector.
#' @param key character, key signature.
#'
#' @return logical
#' @export
#' @seealso \code{\link{note_in_scale}}, \code{\link{is_diatonic}}
#'
#' @examples
#' chord_is_diatonic("ceg ace ce_g", "c")
#' chord_is_diatonic(c("dfa", "df#a"), "d")
chord_is_diatonic <- function(chord, key = "c"){
  .check_chord(chord)
  s <- scale_diatonic(key)
  x <- .uncollapse(chord)
  sapply(x, function(x) all(.pitch_to_note(.split_chord(x)) %in% s), USE.NAMES = FALSE)
}

#' Construct a dyad
#'
#' Construct a dyad given one note, an interval, and a direction.
#'
#' The \code{interval} may be specified by semitones of by common interval name or abbreviation. See examples.
#' For a complete list of valid interval names and abbreviations see \code{\link{mainIntervals}}.
#' \code{key} enforces the use of sharps or flats. This function is based on \code{transpose}.
#' \code{notes} and \code{interval} may be vectors, but must be equal length. Recycling occurs only if one argument is scalar.
#'
#' @param notes character, vector of single notes (not a single space-delimited string).
#' @param interval integer or character vector; semitones or interval ID, respectively. See details.
#' @param reverse logical, reverse the transposition direction. Useful when \code{interval} is character.
#' @param key character, key signature.
#' @param collapse logical, collapse result into a single string ready for phrase construction.
#'
#' @return character
#' @export
#' @seealso \code{\link{mainIntervals}}
#'
#' @examples
#' dyad("a", 4)
#' x <- c("minor third", "m3", "augmented second", "A2")
#' sapply(x, function(x) dyad("a", x))
#' sapply(x, function(x) dyad("c'", x, reverse = TRUE))
#'
#' x <- c("M3", "m3", "m3", "M3", "M3", "m3", "m3")
#' dyad(letters[c(3:7, 1, 2)], x)
#'
#' x <- c("P1", "m3", "M3", "P4", "P5", "P8", "M9")
#' dyad("c", x)
#' dyad("c", x, reverse = TRUE)
dyad <- function(notes, interval, reverse = FALSE, key = "c", collapse = FALSE){
  if(is.character(interval)) interval <- interval_semitones(interval)
  if(any(is.na(interval))) stop("Invalid `interval`.")
  .check_note(notes)
  .keycheck(key)
  if(reverse) interval <- -interval
  nn <- length(notes)
  ni <- length(interval)
  if(nn != ni){
    if(nn != 1 & ni != 1) stop("`notes` and `interval` have unequal lengths both > 1.")
    if(nn == 1) notes <- rep(notes, ni)
    if(ni == 1) interval <- rep(interval, nn)
  }
  f <- function(i, n1, int){
    n1 <- n1[i]
    int <- int[i]
    n2 <- transpose(n1, int, key)
    paste(if(int == 0) n1 else if(int > 0) c(n1, n2) else c(n2, n1), collapse = "")
  }
  x <- sapply(seq_along(notes), f, notes, interval)
  if(collapse) x <- paste(x, collapse = " ")
  x
}

#' Rank, order and sort chords and notes
#'
#' Rank, order and sort chords and notes by various definitions.
#'
#' There are three options for comparing the relative pitch position of chords provided: comparison of the lowest or root note of each chord, the highest pitch note, or taking the mean of all notes in a chord.
#'
#' @param chords character, a noteworthy string, may include individual notes and chords.
#' @param pitch character, how ranking of chords is determined; lowest pitch, mean pitch, or highest pitch.
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
chord_rank <- function(chords, pitch = c("min", "mean", "max"), ...){
  rank(.chord_rank(chords, pitch), ...)
}

#' @export
#' @rdname chord-compare
chord_order <- function(chords, pitch = c("min", "mean", "max"), ...){
  order(.chord_rank(chords, pitch), ...)
}

#' @export
#' @rdname chord-compare
chord_sort <- function(chords, pitch = c("min", "mean", "max"), decreasing = FALSE, ...){
  ord <- chord_order(chords, pitch, ...)
  x <- .uncollapse(chords)[ord]
  if(decreasing) x <- rev(x)
  if(length(chords) == 1) x <- paste0(x, collapse = " ")
  x
}

# nolint start

.chord_rank <- function(x, pitch = c("min", "mean", "max")){
  .check_noteworthy(x)
  pitch <- match.arg(pitch)
  x <- .uncollapse(x)
  x <- sapply(x, function(y){
    y <- sapply(.split_chord(y), function(i) pitch_interval("c", i))
    switch(pitch, "min" = min(y), "mean" = mean(y), "max" = max(y))
  }, USE.NAMES = FALSE)
}

#nolint end

#' Chord constructors
#'
#' These functions construct basic chord string notation from a root \code{note}.
#'
#' Providing a \code{key} signature is used only to ensure flats or sharps for accidentals.
#' An additional set of aliases with efficient names, of the form \code{x*} where \code{*} is a chord modifier abbreviation, is provided to complement the set of \code{chord_*} functions.
#'
#' These functions create standard chords, not the multi-octave spanning types of chords commonly played on guitar.
#'
#' @param note character, chord root note.
#' @param key key signature. See details.
#' @param style character, passed to \code{transpose}.
#' @param collapse logical, collapse result into a single string ready for phrase construction.
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
#' xm("c, f, g,", key = "e_", collapse = TRUE)
chord_min <- function(notes, key = "c", collapse = FALSE, style = "default"){
  semitones <- c(0, 3, 7)
  .chord_prep(notes, semitones, key, collapse, style)
}

#' @export
#' @rdname chords
chord_maj <- function(notes, key = "c", collapse = FALSE, style = "default"){
  semitones <- c(0, 4, 7)
  .chord_prep(notes, semitones, key, collapse, style)
}

#' @export
#' @rdname chords
chord_min7 <- function(notes, key = "c", collapse = FALSE, style = "default"){
  semitones <- c(0, 3, 7, 10)
  .chord_prep(notes, semitones, key, collapse, style)
}

#' @export
#' @rdname chords
chord_dom7 <- function(notes, key = "c", collapse = FALSE, style = "default"){
  semitones <- c(0, 4, 7, 10)
  .chord_prep(notes, semitones, key, collapse, style)
}

#' @export
#' @rdname chords
chord_7s5 <- function(notes, key = "c", collapse = FALSE, style = "default"){
  semitones <- c(0, 4, 8, 10)
  .chord_prep(notes, semitones, key, collapse, style)
}

#' @export
#' @rdname chords
chord_maj7 <- function(notes, key = "c", collapse = FALSE, style = "default"){
  semitones <- c(0, 4, 7, 11)
  .chord_prep(notes, semitones, key, collapse, style)
}

#' @export
#' @rdname chords
chord_min6 <- function(notes, key = "c", collapse = FALSE, style = "default"){
  semitones <- c(0, 3, 7, 9)
  .chord_prep(notes, semitones, key, collapse, style)
}

#' @export
#' @rdname chords
chord_maj6 <- function(notes, key = "c", collapse = FALSE, style = "default"){
  semitones <- c(0, 4, 7, 9)
  .chord_prep(notes, semitones, key, collapse, style)
}

#' @export
#' @rdname chords
chord_dim <- function(notes, key = "c", collapse = FALSE, style = "default"){
  semitones <- c(0, 3, 6)
  .chord_prep(notes, semitones, key, collapse, style)
}

#' @export
#' @rdname chords
chord_dim7 <- function(notes, key = "c", collapse = FALSE, style = "default"){
  semitones <- c(0, 3, 6, 9)
  .chord_prep(notes, semitones, key, collapse, style)
}

#' @export
#' @rdname chords
chord_m7b5 <- function(notes, key = "c", collapse = FALSE, style = "default"){
  semitones <- c(0, 3, 6, 10)
  .chord_prep(notes, semitones, key, collapse, style)
}

#' @export
#' @rdname chords
chord_aug <- function(notes, key = "c", collapse = FALSE, style = "default"){
  semitones <- c(0, 4, 8)
  .chord_prep(notes, semitones, key, collapse, style)
}

#' @export
#' @rdname chords
chord_5 <- function(notes, key = "c", collapse = FALSE, style = "default"){
  semitones <- c(0, 7)
  .chord_prep(notes, semitones, key, collapse, style)
}

#' @export
#' @rdname chords
chord_sus2 <- function(notes, key = "c", collapse = FALSE, style = "default"){
  semitones <- c(0, 2, 7)
  .chord_prep(notes, semitones, key, collapse, style)
}

#' @export
#' @rdname chords
chord_sus4 <- function(notes, key = "c", collapse = FALSE, style = "default"){
  semitones <- c(0, 5, 7)
  .chord_prep(notes, semitones, key, collapse, style)
}

#' @export
#' @rdname chords
chord_dom9 <- function(notes, key = "c", collapse = FALSE, style = "default"){
  semitones <- c(0, 4, 7, 10, 14)
  .chord_prep(notes, semitones, key, collapse, style)
}

#' @export
#' @rdname chords
chord_7s9 <- function(notes, key = "c", collapse = FALSE, style = "default"){
  semitones <- c(0, 4, 7, 10, 15)
  .chord_prep(notes, semitones, key, collapse, style)
}

#' @export
#' @rdname chords
chord_maj9 <- function(notes, key = "c", collapse = FALSE, style = "default"){
  semitones <- c(0, 4, 7, 11, 14)
  .chord_prep(notes, semitones, key, collapse, style)
}

#' @export
#' @rdname chords
chord_add9 <- function(notes, key = "c", collapse = FALSE, style = "default"){
  semitones <- c(0, 4, 7, 14)
  .chord_prep(notes, semitones, key, collapse, style)
}

#' @export
#' @rdname chords
chord_min9 <- function(notes, key = "c", collapse = FALSE, style = "default"){
  semitones <- c(0, 3, 7, 10, 14)
  .chord_prep(notes, semitones, key, collapse, style)
}

#' @export
#' @rdname chords
chord_madd9 <- function(notes, key = "c", collapse = FALSE, style = "default"){
  semitones <- c(0, 3, 7, 14)
  .chord_prep(notes, semitones, key, collapse, style)
}

#' @export
#' @rdname chords
chord_min11 <- function(notes, key = "c", collapse = FALSE, style = "default"){
  semitones <- c(0, 3, 7, 10, 14, 17)
  .chord_prep(notes, semitones, key, collapse, style)
}

#' @export
#' @rdname chords
chord_7s11 <- function(notes, key = "c", collapse = FALSE, style = "default"){
  semitones <- c(0, 4, 7, 10, 18)
  .chord_prep(notes, semitones, key, collapse, style)
}

#' @export
#' @rdname chords
chord_maj7s11 <- function(notes, key = "c", collapse = FALSE, style = "default"){
  semitones <- c(0, 4, 7, 11, 14, 18)
  .chord_prep(notes, semitones, key, collapse, style)
}

#' @export
#' @rdname chords
chord_11 <- function(notes, key = "c", collapse = FALSE, style = "default"){
  semitones <- c(0, 7, 10, 14, 17)
  .chord_prep(notes, semitones, key, collapse, style)
}

#' @export
#' @rdname chords
chord_maj11 <- function(notes, key = "c", collapse = FALSE, style = "default"){
  semitones <- c(0, 4, 7, 11, 14, 17)
  .chord_prep(notes, semitones, key, collapse, style)
}

#' @export
#' @rdname chords
chord_13 <- function(notes, key = "c", collapse = FALSE, style = "default"){
  semitones <- c(0, 4, 7, 10, 14, 21)
  .chord_prep(notes, semitones, key, collapse, style)
}

#' @export
#' @rdname chords
chord_min13 <- function(notes, key = "c", collapse = FALSE, style = "default"){
  semitones <- c(0, 3, 7, 10, 14, 21)
  .chord_prep(notes, semitones, key, collapse, style)
}

#' @export
#' @rdname chords
chord_maj13 <- function(notes, key = "c", collapse = FALSE, style = "default"){
  semitones <- c(0, 4, 7, 11, 14, 17, 21)
  .chord_prep(notes, semitones, key, collapse, style)
}

.chord_prep <- function(notes, semitones, key, collapse, style){
  .check_note(notes)
  .keycheck(key)
  x <- sapply(.uncollapse(notes), function(note){
    paste0(sapply(semitones, function(s) transpose(note, s, key = key, style = style)), collapse = "")
  }, USE.NAMES = FALSE)
  if(collapse) x <- paste0(x, collapse = " ")
  x
}

.dot_arg <- function(dots, .x, default) if(is.null(dots[[.x]])) default else dots[[.x]]

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
xaug <- chord_dim

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
