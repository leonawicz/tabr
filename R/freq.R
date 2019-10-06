#' Pitch-frequency conversion
#'
#' Convert between pitch and frequency.
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
#' @param collapse logical, collapse result into a single string.
#' @param ... other arguments passed to \code{tranpose}, specifically
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
#' freq_pitch(y)
#'
#' identical(as_noteworthy(x), freq_pitch(y, collapse = TRUE))
pitch_freq <- function(notes, a4 = 440){
  a4 * (2 ^ (1 / 12)) ^ (pitch_semitones(notes) - 69)
}

#' @export
#' @name pitch_freq
pitch_semitones <- function(notes){
  .check_note(notes)
  69L + purrr::map_int(.uncollapse(notes), ~pitch_interval("a4", .x))
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
  x <- .uncollapse(notes)
  f <- function(x){
    69L + purrr::map_int(.split_chord(x), ~pitch_interval("a4", .x))
  }
  stats::setNames(lapply(x, f), x)
}

#' @export
#' @name pitch_freq
freq_pitch <- function(freq, a4 = 440, collapse = FALSE, ...){
  n <- round(freq_semitones(freq, a4) - 69)
  x <- sapply(n, function(x, ...) transpose("a4", x, ...))
  if(collapse) x <- paste(x, collapse = " ")
  .asnw(x)
}

#' @export
#' @name pitch_freq
freq_semitones <- function(freq, a4 = 440){
  69 + 12 * log(freq / a4, 2)
}
