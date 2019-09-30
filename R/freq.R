#' Pitch-frequency conversion
#'
#' Convert between pitch and frequency.
#'
#' Frequencies are in Hertz. Values are based on the 12-tone equal-tempered scale. When converting an arbitrary frequency to pitch, it is rounded to the nearest pitch.
#'
#' @param notes character, note string, space-delimited or vector of individual entries.
#' @param fixed_note the fixed note, typically A4, the A above middle C.
#' @param fixed_freq the fixed frequency of \code{fixed_note}, typically 440 Hz.
#' @param freq numeric, frequencies in Hz.
#' @param collapse logical, collapse result into a single string.
#' @param ... other arguments passed to \code{tranpose}, specifically \code{key} and \code{style}.
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
pitch_freq <- function(notes, fixed_note = "a4", fixed_freq = 440){
  .check_fixed(fixed_note, fixed_freq)
  x <- .uncollapse(notes)
  n <- purrr::map_int(x, ~pitch_interval(fixed_note, .x))
  a <- 2 ^ (1 / 12)
  fixed_freq * a ^ n
}

#' @export
#' @name pitch_freq
freq_pitch <- function(freq, fixed_note = "a4", fixed_freq = 440, collapse = FALSE, ...){
  n <- round(freq_semitones(freq, fixed_note, fixed_freq))
  x <- sapply(n, function(x, ...) transpose(fixed_note, x, ...))
  if(collapse) x <- paste(x, collapse = " ")
  .asnw(x)
}

freq_semitones <- function(freq, fixed_note = "a4", fixed_freq = 440){
  .check_fixed(fixed_note, fixed_freq)
  a <- 2 ^ (1 / 12)
  log(freq / fixed_freq, a)
}

.check_fixed <- function(x, y){
  if(length(x) > 1 | any(!is_note(x)))
    stop("Invalid `fixed_note`.", call. = FALSE)
  if(length(y) > 1 | any(y <= 0))
    stop("Invalid `fixed_freq`.", call. = FALSE)
}
