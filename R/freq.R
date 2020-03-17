#' Pitch conversions
#'
#' Convert between pitches, chords, semitones and frequencies.
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
#' @return integer, numeric or noteworthy vector
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
  semitone_freq(pitch_semitones(notes), a4)
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

#' @export
#' @name pitch_freq
semitone_freq <- function(semitones, a4 = 440){
  a4 * (2 ^ (1 / 12)) ^ (semitones - 69)
}

.check_semitone <- function(x){
  if(any(x < 0, na.rm = TRUE) | any(x > 131, na.rm = TRUE))
    stop("Semitones must range from 0 to 131.", call. = FALSE)
}

#' Frequency ratios
#'
#' Obtain frequency ratios data frame.
#'
#' This generic function returns a data frame of frequency ratios from
#' a vector or list of frequencies, a noteworthy object, or a music object. For
#' frequency inputs, a list can be used to represent multiple timesteps.
#' Octave numbering and accidentals are inferred from noteworthy and music
#' objects, but can be specified for frequency. See examples.
#'
#' By default ratios are returned for all combinations of intervals in each
#' chord (\code{ratios = "all"}). \code{ratios = "root"} filters the result to
#' only include chord ratios with respect to the root note of each chord.
#' \code{ratios = "range"} filters to only the chord ratio between the root and
#' highest note.
#'
#' @param x noteworthy or music object, or a numeric vector or list of numeric
#' vectors for frequencies.
#' @param ... additional arguments: \code{ratios}, which is one of \code{"all"}
#' (default), \code{"root"}, or \code{"range"} for filtering results. For
#' frequency input, you may also specify \code{octaves} and \code{accidentals}.
#' See details and examples.
#'
#' @return a tibble data frame
#' @export
#'
#' @examples
#' x <- as_music("c4 e_ g ce_g")
#' (fr <- freq_ratio(x))
#'
#' x <- music_notes(x)
#' identical(fr, freq_ratio(x))
#'
#' x <- chord_freq(x)
#' identical(fr, freq_ratio(x))
#'
#' freq_ratio(x, accidentals = "sharp")
#'
#' freq_ratio(x, ratios = "root")
#'
#' freq_ratio(x, ratios = "range")
freq_ratio <- function(x, ...){
  UseMethod("freq_ratio", x)
}

#' @export
freq_ratio.list <- function(x, ...){
  y <- .freq_ratio_args(...)
  freq_ratio.numeric(x, ratios = y$r, octaves = y$o, accidentals = y$a)
}

#' @export
freq_ratio.noteworthy <- function(x, ...){
  r <- .freq_ratio_args(...)$r
  o <- octave_type(x)
  a <- accidental_type(x)
  freq_ratio.numeric(unname(chord_freq(x)), ratios = r, octaves = o,
                     accidentals = a)
}

#' @export
freq_ratio.music <- function(x, ...){
  freq_ratio.noteworthy(music_notes(x), ...)
}

#' @export
freq_ratio.numeric <- function(x, ...){
  if(!is.list(x)) x <- list(x)
  names(x) <- NULL
  y <- .freq_ratio_args(...)
  purrr::map_dfr(x, .freq_ratio, r = y$r, o = y$o, a = y$a, .id = "timestep") %>%
    dplyr::mutate(
      timestep = as.integer(.data[["timestep"]]),
      notes = as_noteworthy(.data[["notes"]], y$o, y$a, "vector")
    ) %>%
    dplyr::select_at(c("timestep", "notes", "freq1", "freq2", "ratio"))
}

#' @export
freq_ratio.default <- function(x, ...){
  r <- .freq_ratio_args(...)$r
  freq_ratio.noteworthy(as_noteworthy(x), ratios = r)
}

.freq_ratio <- function(x, r, o, a){
  if(length(x) == 1){
    d <- dplyr::tibble(
      notes = as.character(freq_pitch(x, o, a)),
      freq1 = x, freq2 = NA_real_, ratio = NA_real_
    )
    return(d)
  }
  x <- sort(x)
  if(r == "range"){
    x <- range(x)
  }
  x <- utils::combn(x, 2)
  if(r == "root") x <- x[, x[1, ] == x[1, 1]]
  if(!is.matrix(x)) x <- matrix(x, ncol = 1)
  ratio <- x[2, ] / x[1, ]
  f <- function(x, y, o, a){
    gsub("NA", "", paste0(freq_pitch(x, o, a), freq_pitch(y, o, a)))
  }
  dplyr::tibble(freq1 = x[1, ], freq2 = x[2, ], ratio = ratio) %>%
    dplyr::mutate(notes = f(.data[["freq1"]], .data[["freq2"]], o, a))
}


.freq_ratio_args <- function(...){
  x <- list(...)
  r <- if(is.null(x$ratios)) "all" else
    match.arg(x$ratios, c("all", "root", "range"))
  o <- if(is.null(x$octaves)) "tick" else
    match.arg(x$octaves, c("tick", "integer"))
  a <- if(is.null(x$accidentals)) "flat" else
    match.arg(x$accidentals, c("flat", "sharp"))
  list(r = r, o = o, a = a)
}
