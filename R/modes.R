#' Mode helpers
#'
#' Helper functions for working with musical modes.
#'
#' For valid key signatures, see \code{\link{keys}}.
#'
#' Modern modes based on major scales are available by key signature using the
#' \code{mode_*} functions. The seven modes can be listed with \code{modes}.
#' Note strings of proper length can be checked to match against a mode with
#' \code{is_mode}.
#' Modes can be rotated with \code{mode_rotate}, a wrapper around
#' \code{note_rotate}.
#'
#' @param key character, key signature.
#' @param collapse logical, collapse result into a single string ready for
#' phrase construction.
#' @param mode character, which mode.
#' @param notes character, for mode, may be a string of seven notes or a vector
#' or seven one-note strings.
#' @param n integer, degree of rotation.
#' @param ignore_octave logical, strip octave numbering from modes not rooted
#' on C.
#'
#' @return character
#' @export
#' @seealso \code{\link{keys}}, \code{\link{scale-helpers}}
#' @name mode-helpers
#'
#' @examples
#' modes()
#' mode_dorian("c")
#' mode_modern("dorian", "c")
#' mode_modern("dorian", "c", ignore_octave = TRUE)
#'
#' identical(mode_rotate(mode_ionian("c"), 1), mode_dorian("d"))
#' identical(
#'   mode_rotate(mode_ionian("c", ignore_octave = TRUE), 1),
#'   mode_dorian("d", ignore_octave = TRUE)
#' )
#'
#' x <- sapply(modes(), mode_modern, ignore_octave = TRUE)
#' setNames(data.frame(t(x)), as.roman(1:7))
modes <- function(mode = c("all", "major", "minor")){
  mode <- match.arg(mode)
  x <- c("ionian", "dorian", "phrygian", "lydian", "mixolydian", "aeolian",
         "locrian")
  if(mode == "all") return(x)
  if(mode == "major") return(x[c(1, 4, 5)])
  x[c(2, 3, 6, 7)]
}

#' @export
#' @rdname mode-helpers
is_mode <- function(notes, ignore_octave = FALSE){
  notes <- .uncollapse(notes)
  if(length(notes) != 7) return(FALSE)
  key <- .pitch_to_note(notes[1])
  y <- sapply(modes(), mode_modern, key = key, ignore_octave = ignore_octave)
  for(i in seq_len(ncol(y)))
    if(identical(as.character(notes), y[, i])) return(TRUE)
  FALSE
}

#' @export
#' @rdname mode-helpers
mode_rotate <- function(notes, n = 0, ignore_octave = FALSE){
  if(!is_mode(notes, ignore_octave))
    stop("`notes` does not define a valid mode.", call. = FALSE)
  note_rotate(notes, n)
}

#' @export
#' @rdname mode-helpers
mode_modern <- function(mode = "ionian", key = "c", collapse = FALSE,
                        ignore_octave = FALSE){
  f <- switch(mode,
              ionian = mode_ionian, dorian = mode_dorian,
              phrygian = mode_phrygian, lydian = mode_lydian,
              mixolydian = mode_mixolydian, aeolian = mode_aeolian,
              locrian = mode_locrian)
  f(key, collapse, ignore_octave)
}

#' @export
#' @rdname mode-helpers
mode_ionian <- function(key = "c", collapse = FALSE, ignore_octave = FALSE){
  scale_major(key, collapse, ignore_octave)
}

#' @export
#' @rdname mode-helpers
mode_dorian <- function(key = "c", collapse = FALSE, ignore_octave = FALSE){
  x <- scale_major(key, ignore_octave = ignore_octave)
  idx <- c(3, 7)
  x[idx] <- sapply(x[idx], transpose, n = -1, key = key, style = "strip")
  if(collapse) x <- paste0(x, collapse = " ")
  note_set_key(x, key)
}

#' @export
#' @rdname mode-helpers
mode_phrygian <- function(key = "c", collapse = FALSE, ignore_octave = FALSE){
  x <- scale_major(key, ignore_octave = ignore_octave)
  idx <- c(2, 3, 6, 7)
  x[idx] <- sapply(x[idx], transpose, n = -1, key = key, style = "strip")
  if(collapse) x <- paste0(x, collapse = " ")
  note_set_key(x, key)
}

#' @export
#' @rdname mode-helpers
mode_lydian <- function(key = "c", collapse = FALSE, ignore_octave = FALSE){
  x <- scale_major(key, ignore_octave = ignore_octave)
  idx <- 4
  x[idx] <- sapply(x[idx], transpose, n = 1, key = key, style = "strip")
  if(collapse) x <- paste0(x, collapse = " ")
  note_set_key(x, key)
}

#' @export
#' @rdname mode-helpers
mode_mixolydian <- function(key = "c", collapse = FALSE, ignore_octave = FALSE){
  x <- scale_major(key, ignore_octave = ignore_octave)
  idx <- 7
  x[idx] <- sapply(x[idx], transpose, n = -1, key = key, style = "strip")
  if(collapse) x <- paste0(x, collapse = " ")
  note_set_key(x, key)
}

#' @export
#' @rdname mode-helpers
mode_aeolian <- function(key = "c", collapse = FALSE, ignore_octave = FALSE){
  x <- scale_major(key, ignore_octave = ignore_octave)
  idx <- c(3, 6, 7)
  x[idx] <- sapply(x[idx], transpose, n = -1, key = key, style = "strip")
  if(collapse) x <- paste0(x, collapse = " ")
  note_set_key(x, key)
}

#' @export
#' @rdname mode-helpers
mode_locrian <- function(key = "c", collapse = FALSE, ignore_octave = FALSE){
  x <- scale_major(key, ignore_octave = ignore_octave)
  idx <- c(2, 3, 5, 6, 7)
  x[idx] <- sapply(x[idx], transpose, n = -1, key = key, style = "strip")
  if(collapse) x <- paste0(x, collapse = " ")
  note_set_key(x, key)
}
