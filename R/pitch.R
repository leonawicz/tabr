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

.pitch_range <- function(x, octaves = "tick", accidentals = "flat"){
  idx <- .pitch_order(x, octaves, accidentals, na_last = NA)
  len <- length(idx)
  x[c(idx[1], idx[len])]
}

.pitch_sort <- function(x, octaves = "tick", accidentals = "flat",
                        na_last = NA, decreasing = FALSE){
  idx <- .pitch_order(x, octaves, accidentals, na_last)
  if(decreasing) idx <- rev(idx)
  x[idx]
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
