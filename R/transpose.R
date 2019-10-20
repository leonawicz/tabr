#' Transpose pitch
#'
#' Transpose pitch by a number of semitones.
#'
#' This function transposes the pitch of notes in a noteworthy string.
#'
#' Transposing is not currently supported on a phrase object.
#' The notes in a phrase object have already been transformed to LilyPond
#' syntax and mixed with other potentially complex information.
#' Transposing is intended to be done on a string of notes prior to passing it
#' to \code{phrase}. It will work on strings that use either integer or tick
#' mark octave numbering formats and flats or sharps, in any combination.
#' The transposed result conforms according to the function arguments.
#' When integer octaves are returned, all \code{3}s are dropped
#' since the third octave is implicit in LilyPond.
#'
#' When \code{octaves}, \code{accidentals} and \code{key} are \code{NULL},
#' formatting is inferred from \code{notes}. When mixed formats are present,
#' tick format is the default for octave numbering and flats are the default
#' for accidentals.
#'
#' @param notes character, a noteworthy string.
#' @param n integer, positive or negative number of semitones to transpose.
#' @param octaves \code{NULL} or character, \code{"tick"} or \code{"integer"}
#' octave numbering in result.
#' @param accidentals \code{NULL} or character, represent accidentals,
#' \code{"flat"} or \code{"sharp"}.
#' @param key \code{NULL} or character, use a key signature to specify and
#' override \code{accidentals}. Ignored if \code{c} or \code{am}.
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
  format <- if(length(notes) == 1) "space" else "vector"
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
  x <- .uncollapse(notes) %>% .split_chords()
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

.ly_transpose_defs <-
"#(define (naturalize-pitch p)
(let ((o (ly:pitch-octave p))
      (a (* 4 (ly:pitch-alteration p)))
      ;; alteration, a, in quarter tone steps,
      ;; for historical reasons
      (n (ly:pitch-notename p)))
 (cond
   ((and (> a 1) (or (eqv? n 6) (eqv? n 2)))
     (set! a (- a 2))
     (set! n (+ n 1)))
   ((and (< a -1) (or (eqv? n 0) (eqv? n 3)))
     (set! a (+ a 2))
     (set! n (- n 1))))
 (cond
   ((> a 2) (set! a (- a 4)) (set! n (+ n 1)))
   ((< a -2) (set! a (+ a 4)) (set! n (- n 1))))
 (if (< n 0) (begin (set! o (- o 1)) (set! n (+ n 7))))
 (if (> n 6) (begin (set! o (+ o 1)) (set! n (- n 7))))
 (ly:make-pitch o n (/ a 4))))

#(define (naturalize music)
(let ((es (ly:music-property music 'elements))
           (e (ly:music-property music 'element))
      (p (ly:music-property music 'pitch)))
          (if (pair? es)
          (ly:music-set-property!
          music 'elements
          (map naturalize es)))
      (if (ly:music? e)
        (ly:music-set-property!
           music 'element
         (naturalize e)))
         (if (ly:pitch? p)
         (begin
         (set! p (naturalize-pitch p))
         (ly:music-set-property! music 'pitch p)))
  music))

naturalizeMusic =
  #(define-music-function (m)
  (ly:music?)
(naturalize m))

"
