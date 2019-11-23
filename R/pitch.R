#' Create a sequence from pitch notation
#'
#' Create a noteworthy string of a sequence of consecutive pitches.
#'
#' Note that all pitches resulting from the defined sequence must be in the
#' semitone range 0-131 or an error is thrown.
#'
#' If not using a chromatic sequence and \code{x} (or \code{y} if also a pitch)
#' is not part of the key signature or scale, the sequence is internally bound.
#' See examples.
#'
#' Format of accidentals in the result is prioritized by the scale and key, the
#' key when no scale is given, then \code{x} (and \code{y} if also a pitch),
#' and finally defaults to flats if ambiguous.
#'
#' @param x character, valid pitch notation, e.g., \code{"a2"} or \code{"a,"}.
#' @param y character, same as \code{x} for the sequence \code{x:y}. If a
#' number, the length of the sequence from \code{x} and the sign of \code{y}
#' determines the direction.
#' @param key character, key signature for a diatonic sequence.
#' \code{key = NULL} (default) results in a chromatic sequence.
#' @param scale character, if you want to use a different scale in conjunction
#' with the key/root note, you can provide it, e.g.,
#' \code{scale = "harmonic minor"}. Ignored if \code{key = NULL}.
#' @param format character, the timestep delimiter format, \code{"space"} or
#' \code{"vector"}.
#'
#' @return noteworthy
#' @export
#'
#' @examples
#' # chromatic sequence (default)
#' pitch_seq("a,", 13)
#' pitch_seq("c5", -2)
#' pitch_seq("c", "b")
#'
#' # diatonic sequence
#' pitch_seq("c", 8, key = "c")
#' pitch_seq("c", 8, "am")
#' pitch_seq("c#,", "a#'", "am")
#'
#' # combine with alternative scale
#' pitch_seq("a", 8, "am", "harmonic minor")
pitch_seq <- function(x, y, key = NULL, scale = NULL,
                      format = c("space", "vector")){
  format <- match.arg(format)
  x <- as_noteworthy(x)
  if(length(x) > 1) stop("`x` must be a single pitch.", call. = FALSE)
  if(length(y) > 1)
    stop("`y` must be a single pitch or single number.", call. = FALSE)
  if(is.numeric(y)){
    y <- as.integer(y)
    if(y == 0) stop("Cannot have zero timesteps.", call. = FALSE)
  } else {
    y <- as_noteworthy(y)
  }
  .pitch_seq(x, y, key, scale, format)
}

.pitch_seq <- function(x, y, key, scale, format){
  err <- "Pitch semitones must range from 0 to 131."
  z <- c(x, if(is_noteworthy(y)) y)
  o <- if(note_has_integer(z) & !note_has_tick(z)) "integer" else "tick"
  sharp <- note_has_sharp(z) & !note_has_flat(z)
  if(!is.null(key)) sharp <- key_is_sharp(key) | (sharp & !key_is_flat(key))
  x <- .pitch_semitones(as.character(x))
  if(is.na(x)) stop(err, call. = FALSE)

  if(is.null(key)){
    notes0 <- scale_chromatic(sharp = sharp)
  } else {
    .keycheck(key)
    if(!is.null(scale)){
      s <- paste0("scale_", gsub(" ", "_", scale))
      .check_scale_fun(s)
      notes0 <- .pitch_to_note(do.call(s, list(key = key)))
      if(note_has_sharp(notes0)) sharp <- TRUE
    } else {
      notes0 <- .pitch_to_note(scale_diatonic(key))
    }
  }
  a <- if(sharp) "sharp" else "flat"

  if(is.numeric(y)){
    if(is.null(key)){
      x <- semitone_pitch(seq(x, by = sign(y), length.out = abs(y)),
                          accidentals = a)
    } else {
      ap <- if(sharp) .all_pitches_tick_sharp else .all_pitches_tick
      idx <- c(0:131)[.pitch_to_note(ap) %in% notes0]
      if(y > 0){
        i <- which(idx - x >= 0)[1]
        x <- idx[i:(i + y - 1)]
      } else {
        i <- rev(which(idx - x <= 0))[1]
        x <- idx[i:(i + y + 1)]
      }
      x <- semitone_pitch(x, accidentals = a)
    }
  } else {
    y <- .pitch_semitones(as.character(y))
    if(is.na(y)) stop(err, call. = FALSE)
    x <- semitone_pitch(seq(x, y), accidentals = a)
    notes <- .pitch_to_note(x)
    x <- x[notes %in% notes0]
  }
  .asnw(x, o, a, format)
}

.pitch_to_note <- function(x, tie = TRUE){
  pat <- if(tie) "[0-9,'~]" else "[0-9,']"
  gsub(pat, "", x)
}

.pitch_to_octave <- function(x){
  x <- .octave_to_int(x)
  x <- gsub(".*(\\d+).*", "\\1", x)
  idx <- note_is_integer(x)
  y <- rep(3L, length(x))
  if(any(idx)) y[idx] <- as.integer(gsub(".*(\\d)", "\\1", x[idx]))
  y
}

.pitch_min <- function(x, octaves = "tick", accidentals = "flat"){
  idx <- .pitch_order(x, octaves, accidentals, na_last = NA)
  len <- length(idx)
  x[idx[1]]
}

.pitch_max <- function(x, octaves = "tick", accidentals = "flat"){
  idx <- .pitch_order(x, octaves, accidentals, na_last = NA)
  len <- length(idx)
  x[idx[len]]
}

.pitch_order <- function(x, octaves = "tick", accidentals = "flat",
                         na_last = TRUE){
  idx <- .pitch_semitones(x, octaves, accidentals)
  order(idx, na.last = na_last)
}

.pitch_semitones <- function(x, octaves = "tick", accidentals = "flat"){
  match(.pitch_conform(x, octaves, accidentals),
        .all_pitches(octaves, accidentals)) - 1L
}

.all_pitches <- function(octaves = "tick", accidentals = "flat"){
  if(octaves == "tick"){
    x <- if(accidentals == "flat") .all_pitches_tick else
      .all_pitches_tick_sharp
  } else {
    x <- if(accidentals == "flat") .all_pitches_integer else
      .all_pitches_integer_sharp
  }
  x
}

.pitch_conform <- function(x, octaves = "tick", accidentals = "flat"){
  if(octaves == "tick"){
    idx <- note_is_integer(x)
    if(any(idx)) x[idx] <- .octave_to_tick(x[idx])
  } else {
    idx <- note_is_tick(x)
    if(any(idx)) x[idx] <- .octave_to_int(x[idx])
  }
  if(accidentals == "flat"){
    idx <- note_is_sharp(x)
    if(any(idx)) x[idx] <- .sharp_to_flat(x[idx])
  } else {
    idx <- note_is_flat(x)
    if(any(idx)) x[idx] <- .flat_to_sharp(x[idx])
  }
  x
}

.pitch_interval <- function(x, y){
  .pitch_semitones(y) - .pitch_semitones(x)
}
