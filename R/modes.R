#' Mode helpers
#'
#' Helper functions for working with musical modes.
#'
#' For valid key signatures, see \code{\link{keys}}.
#'
#' Modern modes based on major scales are available by key signature using the \code{mode_*} functions. The seven modes can be listed with \code{modes}. Note strings of proper length can be checked to match against a mode with \code{is_mode}.
#' Modes can be rotated with \code{mode_rotate}, a wrapper around \code{note_rotate}.
#'
#' @param key character, key signature.
#' @param collapse logical, collapse result into a single string ready for phrase construction.
#' @param mode character, which mode.
#' @param notes character, for mode, may be a string of seven notes or a vector or seven one-note strings.
#' @param n integer, degree of rotation.
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
#'
#' identical(mode_rotate(mode_ionian("c"), 1), mode_dorian("d"))
#'
#' setNames(data.frame(t(sapply(modes(), mode_modern))), as.roman(1:7))

modes <- function(mode = c("all", "major", "minor")){
  mode <- match.arg(mode)
  x <- c("ionian", "dorian", "phrygian", "lydian", "mixolydian", "aeolian", "locrian")
  if(mode == "all") return(x)
  if(mode == "major") return(x[c(1, 4, 5)])
  x[c(2, 3, 6, 7)]
}

#' @export
#' @rdname mode-helpers
is_mode <- function(notes){
  n <- length(notes)
  if(!n %in% c(1, 7)) return(FALSE)
  if(n == 1) notes <- strsplit(notes, " ")[[1]]
  key <- notes[1]
  y <- sapply(modes(), mode_modern, key = key)
  for(i in 1:ncol(y)) if(identical(notes, y[, i])) return(TRUE)
  FALSE
}

#' @export
#' @rdname mode-helpers
mode_rotate <- function(notes, n = 0){
  if(!is_mode(notes)) stop("`notes` does not define a valid mode.")
  note_rotate(notes, n)
}

#' @export
#' @rdname mode-helpers
mode_modern <- function(mode = "ionian", key = "c", collapse = FALSE){
  f <- switch(mode,
              ionian = mode_ionian, dorian = mode_dorian, phrygian = mode_phrygian,
              lydian = mode_lydian, mixolydian = mode_mixolydian,
              aeolian = mode_aeolian, locrian = mode_locrian)
  f(key, collapse)
}

#' @export
#' @rdname mode-helpers
mode_ionian <- function(key = "c", collapse = FALSE){
  scale_major(key, collapse)
}

#' @export
#' @rdname mode-helpers
mode_dorian <- function(key = "c", collapse = FALSE){
  x <- scale_major(key)
  idx <- c(3, 7)
  x[idx] <- sapply(x[idx], transpose, n = -1, key = key, style = "strip")
  if(collapse) x <- paste0(x, collapse = " ")
  x
}

#' @export
#' @rdname mode-helpers
mode_phrygian <- function(key = "c", collapse = FALSE){
  x <- scale_major(key)
  idx <- c(2, 3, 6, 7)
  x[idx] <- sapply(x[idx], transpose, n = -1, key = key, style = "strip")
  if(collapse) x <- paste0(x, collapse = " ")
  x
}

#' @export
#' @rdname mode-helpers
mode_lydian <- function(key = "c", collapse = FALSE){
  x <- scale_major(key)
  idx <- 4
  x[idx] <- sapply(x[idx], transpose, n = 1, key = key, style = "strip")
  if(collapse) x <- paste0(x, collapse = " ")
  x
}

#' @export
#' @rdname mode-helpers
mode_mixolydian <- function(key = "c", collapse = FALSE){
  x <- scale_major(key)
  idx <- 7
  x[idx] <- sapply(x[idx], transpose, n = -1, key = key, style = "strip")
  if(collapse) x <- paste0(x, collapse = " ")
  x
}

#' @export
#' @rdname mode-helpers
mode_aeolian <- function(key = "c", collapse = FALSE){
  x <- scale_major(key)
  idx <- c(3, 6, 7)
  x[idx] <- sapply(x[idx], transpose, n = -1, key = key, style = "strip")
  if(collapse) x <- paste0(x, collapse = " ")
  x
}

#' @export
#' @rdname mode-helpers
mode_locrian <- function(key = "c", collapse = FALSE){
  x <- scale_major(key)
  idx <- c(2, 3, 5, 6, 7)
  x[idx] <- sapply(x[idx], transpose, n = -1, key = key, style = "strip")
  if(collapse) x <- paste0(x, collapse = " ")
  x
}
