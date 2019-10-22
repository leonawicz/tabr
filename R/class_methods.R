#' Implemented methods for tabr classes
#'
#' Several methods are implemented for the classes \code{noteworthy},
#' \code{noteinfo}, and \code{music}.
#'
#' @details
#' In addition to custom print and summary methods, the following methods have
#' been implemented for all three classes: \code{[}, \code{[<-}, \code{[[},
#' \code{[[<-}, \code{length}, \code{c}, \code{rep}, \code{rev}, \code{head}
#' and \code{tail}.
#'
#' @section Methods \code{length} and \code{c}:
#' The implementation of \code{length} is equivalent to \code{n_steps}. They
#' access the same attribute, returning the number of timesteps in the object.
#' This gives the same result even when the underlying string is in
#' space-delimited format. To obtain the character string length, coerce with
#' \code{as.character} or any other function that would have the same effect.
#'
#' The implementation of \code{c} for these classes is strict and favors the
#' object class in question. This is different from how \code{c} might normally
#' behave, coercing objects of different types such as numeric and character to
#' character.
#'
#' For these three classes, \code{c} is strict in that it will return
#' an error if attempting to concatenate one of these classes with any other
#' class besides character. This includes each other. While it would be
#' possible to coerce a music object down to a \code{noteworthy} object or a
#' \code{noteinfo} object, this is the opposite of the aggressive coercion
#' these classes are intended to have with \code{c} so this is not done.
#'
#' While other classes such as numeric immediately return an error, any
#' concatenation with character strings attempts to coerce each character
#' string present to the given class. If coercion fails for any character class
#' object, the usual error is returned concerning invalid notes or note info
#' present. If coercion succeeds for all character strings, the result of
#' \code{c} is to concatenate the timesteps of all objects passed to it. The
#' output is a new \code{noteworthy}, \code{noteinfo} or\code{music} object.
#'
#' @section Methods \code{rep} \code{rev} \code{head} and \code{tail}:
#' The \code{rep} function is similar to \code{c} except that it never has to
#' consider other classes. You could pass a vector of objects to \code{rep},
#' but doing so with \code{c} will already have resolved all objects to the
#' single class. Again, what matters is not the underlying length or elements in
#' the character vector the class is built upon, but the timesteps.
#' \code{rep} will extend \code{x} in terms of timesteps. You can also provide
#' the \code{each} or \code{times} arguments.
#'
#' \code{rev}, \code{head} and \code{tail} work similarly, based on the
#' sequence of timesteps, not the character vector length.
#'
#' Remember that this accounts not only for vectors of length one that contain
#' multiple timesteps in space-delimited time format, but also that multiple
#' timesteps can be condensed even in space-delimited time format with the
#' \code{*} expansion operator.
#' For example, \code{"a'*4 b'*2"} has six timesteps in this form as well as in
#' vector form. The object length is neither one nor two. All of these generic
#' method implementations work in this manner.
#'
#' @section Square brackets:
#' Single and double square bracket subsetting by index work similarly to what
#' occurs with lists. Single bracket subsetting returns the same object,
#' but only containing the indexed timesteps. Double bracket subsetting only
#' operates on a single timestep and extracts the character string value.
#'
#' For assignment, single and double brackets change the value at timesteps and
#' return the same object, but again double brackets only allow indexing a
#' single timestep. Double bracket indexing is mostly useful for combining the
#' steps of extracting a single value and discarding the special class in one
#' command.
#'
#' @param x object
#' @param i index
#' @param value values to assign at index.
#'
#' @name tabr-methods
#' @seealso \code{\link{note-metadata}}
#' @importFrom utils head tail
#'
#' @examples
#' # noteworthy class examples
#' x <- as_noteworthy("a, b, c ce_g d4f#4a4")
#' x
#' x[3:4]
#' x[-2]
#' x[2] <- paste0(transpose(x[2], 1), "~")
#' x
#' length(x) # equal to number of timesteps
#' c(x, x)
#' tail(rep(x, times = c(1, 2, 1, 3, 1)))
#'
#' # noteinfo class examples
#' x <- as_noteinfo(c("4-", "t8(", "t8)", "t8x", "8^", "16"))
#' x
#' x[2:4]
#' x[-1]
#' x[5:6] <- c("16^", "8")
#' x
#' x[x == "4-"]
#' c(x[1], x[2]) == c(x[1:2])
#' head(rep(x, each = 2))
#'
#' # music class examples
#' x <- as_music("c,~4 c,1 c'e_'g'4]*4")
#' x
#' x[1:3]
#' x[-c(1:2)]
#' x[3:6] <- "c'e'g'8"
#' x
#' c(x[1], x[1]) == x[c(1, 1)]
#' rev(x)
#'
#' x[[3]]
#' x[[3]] <- "b_t8"
#' x
NULL

#' @export
`[.noteworthy` <- function(x, i){
  if(all(i == 0)) stop("Cannot have zero timesteps.", call. = FALSE)
  o <- octave_type(x)
  a <- accidental_type(x)
  format <- if(time_format(x) == "space-delimited time") "space" else "vector"
  x <- .Primitive("[")(.uncollapse(x), i)
  .asnw(x, o, a, format)
}

#' @export
`[.noteinfo` <- function(x, i){
  if(all(i == 0)) stop("Cannot have zero timesteps.", call. = FALSE)
  format <- if(time_format(x) == "space-delimited time") "space" else "vector"
  x <- .Primitive("[")(.uncollapse(x), i)
  .asni(x, format)
}

#' @export
`[.music` <- function(x, i){
  if(all(i == 0)) stop("Cannot have zero timesteps.", call. = FALSE)
  a <- accidental_type(x)
  tsig <- attr(x, "tsig")
  format <- if(time_format(x) == "space-delimited time") "space" else "vector"
  x <- .Primitive("[")(.uncollapse(x), i)
  notes <- gsub("^([a-grs_#~,']+).*", "\\1", x)
  info <- gsub("^[a-grs_#~,']+(.*)", "\\1", x)
  .asmusic(notes, info, a, tsig, format)
}

#' @export
`[<-.noteworthy` <- function(x, i, value){
  o <- octave_type(x)
  a <- accidental_type(x)
  format <- if(time_format(x) == "space-delimited time") "space" else "vector"
  x <- .Primitive("[<-")(.uncollapse(x), i, value)
  .asnw(x, o, a, format)
}

#' @export
`[<-.noteinfo` <- function(x, i, value){
  format <- if(time_format(x) == "space-delimited time") "space" else "vector"
  x <- .Primitive("[<-")(.uncollapse(x), i, value)
  .asni(x, format)
}

#' @export
`[<-.music` <- function(x, i, value){
  a <- accidental_type(x)
  tsig <- attr(x, "tsig")
  format <- if(time_format(x) == "space-delimited time") "space" else "vector"
  x <- .Primitive("[<-")(.uncollapse(x), i, value)
  notes <- gsub("^([a-grs_#~,']+).*", "\\1", x)
  info <- gsub("^[a-grs_#~,']+(.*)", "\\1", x)
  .asmusic(notes, info, a, tsig, format)
}

#' @export
`[[.noteworthy` <- function(x, i){
  .uncollapse(x)[[i]]
}

#' @export
`[[.noteinfo` <- function(x, i){
  .uncollapse(x)[[i]]
}

#' @export
`[[.music` <- function(x, i){
  .uncollapse(x)[[i]]
}

#' @export
`[[<-.noteworthy` <- function(x, i, value){
  o <- octave_type(x)
  a <- accidental_type(x)
  format <- if(time_format(x) == "space-delimited time") "space" else "vector"
  x <- .Primitive("[[<-")(.uncollapse(x), i, value)
  .asnw(x, o, a, format)
}

#' @export
`[[<-.noteinfo` <- function(x, i, value){
  format <- if(time_format(x) == "space-delimited time") "space" else "vector"
  x <- .Primitive("[[<-")(.uncollapse(x), i, value)
  .asni(x, format)
}

#' @export
`[[<-.music` <- function(x, i, value){
  a <- accidental_type(x)
  tsig <- attr(x, "tsig")
  format <- if(time_format(x) == "space-delimited time") "space" else "vector"
  x <- .Primitive("[[<-")(.uncollapse(x), i, value)
  notes <- gsub("^([a-grs_#~,']+).*", "\\1", x)
  info <- gsub("^[a-grs_#~,']+(.*)", "\\1", x)
  .asmusic(notes, info, a, tsig, format)

}

#' @export
length.noteworthy <- function(x){
  attr(x, "steps")
}

#' @export
length.noteinfo <- function(x){
  attr(x, "steps")
}

#' @export
length.music <- function(x){
  attr(x, "steps")
}

#' @export
c.noteworthy <- function(...){
  x <- list(...)
  cl <- sapply(lapply(x, class), "[", 1)
  if(any(!cl %in% c("noteworthy", "character")))
    stop("Cannot concatenate incompatible classes with 'noteworthy'.",
         call. = FALSE)
  idx <- which(cl == "character")
  if(length(idx)) x[idx] <- lapply(x[idx], as_noteworthy)
  o <- sapply(x, octave_type)
  a <- sapply(x, accidental_type)
  format <- sapply(x, time_format)
  o <- if(!any(o == "tick")) "integer" else "tick"
  a <- if(!any(a == "flat")) "sharp" else "flat"
  format <- if(!any(format == "space-delimited time")) "vector" else "space"
  x <- purrr::map_chr(x, ~paste(as.character(.x), collapse = " ")) %>%
    paste(collapse = " ")
  .asnw(x, o, a, format)
}

#' @export
c.noteinfo <- function(...){
  x <- list(...)
  cl <- sapply(lapply(x, class), "[", 1)
  if(any(!cl %in% c("noteinfo", "character")))
    stop("Cannot concatenate incompatible classes with 'noteinfo'.",
         call. = FALSE)
  idx <- which(cl == "character")
  if(length(idx)) x[idx] <- lapply(x[idx], as_noteinfo)
  format <- sapply(x, time_format)
  format <- if(!any(format == "space-delimited time")) "vector" else "space"
  x <- purrr::map_chr(x, ~paste(as.character(.x), collapse = " ")) %>%
    paste(collapse = " ")
  .asni(x, format)
}

#' @export
c.music <- function(...){
  x <- list(...)
  cl <- sapply(lapply(x, class), "[", 1)
  if(any(!cl %in% c("music", "character")))
    stop("Cannot concatenate incompatible classes with 'music'.",
         call. = FALSE)
  idx <- which(cl == "character")
  if(length(idx)) x[idx] <- lapply(x[idx], as_music)
  tsig <- sapply(x, music_tsig)
  a <- sapply(x, accidental_type)
  format <- sapply(x, time_format)
  tsig <- if(any(tsig != tsig[1])){
    warning("Time signature is inconsistent. Only the first is used.")
  }
  a <- if(!any(a == "flat")) "sharp" else "flat"
  format <- if(!any(format == "space-delimited time")) "vector" else "space"
  x <- purrr::map_chr(x, ~paste(as.character(.x), collapse = " ")) %>%
    paste(collapse = " ") %>%
    .check_music_split()
  .asmusic(x$notes, x$info, a, tsig[1], format)
}

#' @export
rep.noteworthy <- function(x, ...){
  o <- octave_type(x)
  a <- accidental_type(x)
  format <- time_format(x)
  format <- if(format == "space-delimited time") "space" else "vector"
  x <- .uncollapse(x)
  y <- list(...)
  if(is.null(y$each) & is.null(y$times)){
    x <- rep(x, ...)
  } else if(!is.null(y$each)){
    x <- rep(x, each = y$each)
  } else {
    x <- rep(x, times = y$times)
  }
  if(!length(x)) stop("Cannot have zero timesteps.", call. = FALSE)
  if(format == "space") x <- paste0(x, collapse = " ")
  .asnw(x, o, a, format)
}

#' @export
rep.noteinfo <- function(x, ...){
  format <- time_format(x)
  format <- if(format == "space-delimited time") "space" else "vector"
  x <- .uncollapse(x)
  y <- list(...)
  if(is.null(y$each) & is.null(y$times)){
    x <- rep(x, ...)
  } else if(!is.null(y$each)){
    x <- rep(x, each = y$each)
  } else {
    x <- rep(x, times = y$times)
  }
  if(!length(x)) stop("Cannot have zero timesteps.", call. = FALSE)
  if(format == "space") x <- paste0(x, collapse = " ")
  .asni(x, format)
}

#' @export
rep.music <- function(x, ...){
  a <- accidental_type(x)
  format <- time_format(x)
  tsig <- music_tsig(x)
  format <- if(format == "space-delimited time") "space" else "vector"
  x <- .uncollapse(x)
  notes <- rep(.music_notes(x, format), ...)
  info <- rep(.music_info(x, format), ...)
  .asmusic(notes, info, a, tsig, format)
}

#' @export
rev.noteworthy <- function(x){
  o <- octave_type(x)
  a <- accidental_type(x)
  format <- time_format(x)
  format <- if(format == "space-delimited time") "space" else "vector"
  x <- rev(.uncollapse(x))
  if(format == "space") x <- paste0(x, collapse = " ")
  .asnw(x, o, a, format)
}

#' @export
rev.noteinfo <- function(x){
  format <- time_format(x)
  format <- if(format == "space-delimited time") "space" else "vector"
  x <- rev(.uncollapse(x))
  if(format == "space") x <- paste0(x, collapse = " ")
  .asni(x, format)
}

#' @export
rev.music <- function(x){
  a <- accidental_type(x)
  tsig <- music_tsig(x)
  format <- time_format(x)
  format <- if(format == "space-delimited time") "space" else "vector"
  x <- rev(.uncollapse(x))
  notes <- .music_notes(x, format)
  info <- .music_info(x, format)
  .asmusic(notes, info, a, tsig, format)
}

#' @export
head.noteworthy <- function(x, ...){
  o <- octave_type(x)
  a <- accidental_type(x)
  format <- time_format(x)
  format <- if(format == "space-delimited time") "space" else "vector"
  x <- utils::head(.uncollapse(x), ...)
  if(format == "space") x <- paste0(x, collapse = " ")
  .asnw(x, o, a, format)
}

#' @export
head.noteinfo <- function(x, ...){
  format <- time_format(x)
  format <- if(format == "space-delimited time") "space" else "vector"
  x <- utils::head(.uncollapse(x), ...)
  if(format == "space") x <- paste0(x, collapse = " ")
  .asni(x, format)
}

#' @export
head.music <- function(x, ...){
  a <- accidental_type(x)
  tsig <- music_tsig(x)
  format <- time_format(x)
  format <- if(format == "space-delimited time") "space" else "vector"
  x <- utils::head(.uncollapse(x), ...)
  notes <- .music_notes(x, format)
  info <- .music_info(x, format)
  .asmusic(notes, info, a, tsig, format)
}

#' @export
tail.noteworthy <- function(x, ...){
  o <- octave_type(x)
  a <- accidental_type(x)
  format <- time_format(x)
  format <- if(format == "space-delimited time") "space" else "vector"
  x <- utils::tail(.uncollapse(x), ...)
  if(format == "space") x <- paste0(x, collapse = " ")
  .asnw(x, o, a, format)
}

#' @export
tail.noteinfo <- function(x, ...){
  format <- time_format(x)
  format <- if(format == "space-delimited time") "space" else "vector"
  x <- utils::tail(.uncollapse(x), ...)
  if(format == "space") x <- paste0(x, collapse = " ")
  .asni(x, format)
}

#' @export
tail.music <- function(x, ...){
  a <- accidental_type(x)
  tsig <- music_tsig(x)
  format <- time_format(x)
  format <- if(format == "space-delimited time") "space" else "vector"
  x <- utils::tail(.uncollapse(x), ...)
  notes <- .music_notes(x, format)
  info <- .music_info(x, format)
  .asmusic(notes, info, a, tsig, format)
}
