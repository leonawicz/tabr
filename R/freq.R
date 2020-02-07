#' Pitch conversions
#'
#' Convert between pitch and other quantities.
#'
#' Frequencies are in Hertz. Values are based on the 12-tone equal-tempered
#' scale. When converting an arbitrary frequency to pitch, it is rounded to the
#' nearest pitch.
#' \code{pitch_freq} and \code{pitch_semitones} strictly accept single notes in
#' noteworthy strings and return numeric vectors.
#' \code{chord_freq} and \code{chord_semitones} accept any noteworthy string
#' and always return a list. These are provided so that all functions are
#' type-safe. See examples.
#'
#' @param notes character, noteworthy string, space-delimited or vector of
#' individual entries. See details.
#' @param a4 the fixed frequency of the A above middle C, typically 440 Hz.
#' @param freq numeric vector, frequencies in Hz.
#' @param semitones integer values of pitches.
#' @param octaves \code{NULL} or character, \code{"tick"} or \code{"integer"}
#' octave numbering in result.
#' @param accidentals \code{NULL} or character, represent accidentals,
#' \code{"flat"} or \code{"sharp"}.
#' @param collapse logical, collapse result into a single string.
#' \code{key} and \code{style}.
#'
#' @return numeric vector
#' @export
#'
#' @examples
#' x <- "a e4 a4 e5 a5"
#' y <- pitch_freq(x)
#' y
#'
#' freq_semitones(y)
#' freq_pitch(y)
#'
#' identical(as_noteworthy(x), freq_pitch(y, "integer", collapse = TRUE))
#'
#' s <- pitch_semitones(x)
#' s
#' semitone_pitch(s)
#'
#' x <- "a, a,c#e"
#' chord_semitones(x)
#' chord_freq(x)
pitch_freq <- function(notes, a4 = 440){
  a4 * (2 ^ (1 / 12)) ^ (pitch_semitones(notes) - 69)
}

#' @export
#' @name pitch_freq
pitch_semitones <- function(notes){
  .check_note(notes)
  69L + purrr::map_int(gsub("~", "", .uncollapse(notes)),
                       ~.get_pitch_interval("a4", .x))
}

#' @export
#' @name pitch_freq
chord_freq <- function(notes, a4 = 440){
  x <- lapply(chord_semitones(notes),
              function(x) a4 * (2 ^ (1 / 12)) ^ (x - 69))
  stats::setNames(x, .uncollapse(notes))
}

#' @export
#' @name pitch_freq
chord_semitones <- function(notes){
  .check_noteworthy(notes)
  x <- gsub("~", "", .uncollapse(notes))
  f <- function(x){
    69L + purrr::map_int(.split_chords(x), ~.get_pitch_interval("a4", .x))
  }
  stats::setNames(lapply(x, f), x)
}

#' @export
#' @name pitch_freq
freq_pitch <- function(freq, octaves = c("tick", "integer"),
                       accidentals = c("flat", "sharp"), collapse = FALSE,
                       a4 = 440){
  o <- match.arg(octaves)
  a <- match.arg(accidentals)
  n <- round(freq_semitones(freq, a4))
  semitone_pitch(n, o, a, collapse)
}

#' @export
#' @name pitch_freq
freq_semitones <- function(freq, a4 = 440){
  69 + 12 * log(freq / a4, 2)
}

#' @export
#' @name pitch_freq
semitone_pitch <- function(semitones, octaves = c("tick", "integer"),
                           accidentals = c("flat", "sharp"), collapse = FALSE){
  .check_semitone(semitones)
  o <- match.arg(octaves)
  a <- match.arg(accidentals)
  x <- .all_pitches_tick[semitones + 1]
  if(collapse) x <- paste(x, collapse = " ")
  .asnw(x, o, a)
}

.check_semitone <- function(x){
  if(any(x < 0, na.rm = TRUE) | any(x > 131, na.rm = TRUE))
    stop("Semitones must range from 0 to 131.", call. = FALSE)
}
