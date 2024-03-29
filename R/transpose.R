#' Transpose pitch
#'
#' Transpose pitch by a number of semitones.
#'
#' This function transposes the pitch of notes in a noteworthy string.
#'
#' Transposing is not currently supported on a phrase object. The notes in a
#' phrase object have already been transformed to LilyPond syntax and mixed with
#' other potentially complex information. Transposing is intended to be done on
#' a string of notes prior to passing it to `phrase()`. It will work on strings
#' that use either integer or tick mark octave numbering formats and flats or
#' sharps, in any combination. The transposed result conforms according to the
#' function arguments. When integer octaves are returned, all `3`s are dropped
#' since the third octave is implicit in LilyPond.
#'
#' When `octaves`, `accidentals` and `key` are `NULL`, formatting is inferred
#' from `notes`. When mixed formats are present, tick format is the default for
#' octave numbering and flats are the default for accidentals.
#'
#' @param notes character, a noteworthy string.
#' @param n integer, positive or negative number of semitones to transpose.
#' @param octaves `NULL` or character, `"tick"` or `"integer"` octave numbering
#' in result.
#' @param accidentals `NULL` or character, represent accidentals, `"flat"` or
#' `"sharp"`.
#' @param key `NULL` or character, use a key signature to specify and override
#' `accidentals`. Ignored if `c` or `am`.
#'
#' @return character
#' @export
#'
#' @examples
#' transpose("a_3 b_4 c5", 0)
#' tp("a_3 b_4 c5", -1)
#' tp("a_3 b_4 c5", 1)
#' tp("a#3 b4 c#5", 11)
#' tp("a#3 b4 c#5", 12)
#' tp("r s a#3 b4 c#5", 13)
#' tp("a b' c#''", 2, "integer", "flat")
#' tp("a, b ceg", 2, "tick", "sharp")
transpose <- function(notes, n = 0, octaves = NULL, accidentals = NULL,
                      key = NULL){
  if(inherits(notes, "phrase"))
    stop("`notes` must be a noteworthy string, not a phrase object.",
         call. = FALSE)
  .check_noteworthy(notes)
  .check_octaves_arg(octaves)
  .check_accidentals_arg(accidentals)
  format <- if(length(as.character(notes)) == 1) "space" else "vector"
  if(is.null(octaves)) octaves <- .infer_octave_type(notes)
  if(is.null(accidentals)) accidentals <- .infer_accidentals(notes)
  if(!is.null(key)){
    if(key %in% c("flat", "sharp")){
      accidentals <- key
    } else {
      .keycheck(key)
      if(!key %in% c("c", "am")){
        accidentals <- if(key_is_flat(key)) "flat" else "sharp"
      }
    }
  }
  x <- .uncollapse(notes) |> .split_chords()
  idx <- grep("~", x)
  if(length(idx)) x[idx] <- gsub("~", "", x[idx])
  r_idx <- which(x == "r")
  s_idx <- which(x == "s")
  x <- semitone_pitch(.pitch_semitones(x) + n)
  if(length(idx)) x[idx] <- paste0(x[idx], "~")
  if(length(r_idx)) x[r_idx] <- "r"
  if(length(r_idx)) x[s_idx] <- "s"
  x[is.na(x)] <- " "
  x <- paste(x, collapse = "")
  .asnw(x, octaves, accidentals, format)
}

.sharp_to_flat <- function(x){
  len <- length(x)
  x <- .split_chords(x)
  note <- .pitch_to_note(x, tie = FALSE)
  idx <- grep("#", note)
  f <- function(x){
    switch(gsub("~", "", x),
           "c#" = "d_", "d#" = "e_", "f#" = "g_", "g#" = "a_", "a#" = "b_")
  }
  if(length(idx))
    x[idx] <- purrr::map2_chr(x[idx], note[idx], ~gsub("[a-g]#", f(.y), .x))
  x <- paste(x, collapse = "")
  if(len == 1) x else .uncollapse(x)
}

.flat_to_sharp <- function(x){
  len <- length(x)
  x <- .split_chords(x)
  note <- .pitch_to_note(x, tie = FALSE)
  idx <- grep("_", note)
  f <- function(x){
    switch(gsub("~", "", x),
           "d_" = "c#", "e_" = "d#", "g_" = "f#", "a_" = "g#" , "b_" = "a#")
  }
  if(length(idx))
    x[idx] <- purrr::map2_chr(x[idx], note[idx], ~gsub("[a-g]_", f(.y), .x))
  x <- paste(x, collapse = "")
  if(len == 1) x else .uncollapse(x)
}

#' @export
#' @rdname transpose
tp <- transpose
