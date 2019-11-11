#' Summary of implemented S3 generic methods
#'
#' Several methods are implemented for the classes \code{noteworthy},
#' \code{noteinfo}, \code{music} and \code{lyrics}.
#' See further below for details on limited implementations for the
#' \code{phrase} class.
#'
#' @details
#' In addition to custom print and summary methods, the following methods have
#' been implemented for all four classes: \code{[}, \code{[<-}, \code{[[},
#' \code{[[<-}, \code{length}, \code{c}, \code{rep}, \code{rev}, \code{head}
#' and \code{tail}. Logical operators are also implemented for noteworthy
#' strings.
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
#' For these four classes, \code{c} is strict in that it will return
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
#' Single and double bracket subsetting by index work similarly to what
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
#' @section Limited phrase implementations:
#' Methods implemented for the \code{phrase} are limited to \code{c} and
#' \code{rep}. Due to the complex LilyPond syntax, applying most of the
#' functions above directly to phrases is problematic. \code{c} is implemented
#' like it is for the other classes. \code{rep} is restricted in that it can
#' only repeat the entire phrase sequence, not the timesteps within. However,
#' you can convert a phrase class back to \code{noteworthy} and \code{noteinfo}
#' objects (under reasonable conditions). See \code{\link{notify}}.
#'
#' @param x object.
#' @param i index.
#' @param value values to assign at index.
#' @param ... additional arguments.
#'
#' @name tabr-methods
#' @seealso \code{\link{note-logic}}, \code{\link{note-metadata}}
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

#' Single bracket methods for tabr classes
#'
#' Single bracket indexing and assignment. See \code{\link{tabr-methods}} for
#' more details on methods for tabr classes.
#'
#' @param x object.
#' @param i index.
#' @param value values to assign at index.
#'
#' @name single-bracket
#' @seealso \code{\link{tabr-methods}}, \code{\link{note-metadata}}
#' @export
#'
#' @examples
#' # noteworthy class examples
#' x <- as_noteworthy("a, b, c ce_g d4f#4a4")
#' x[3:4]
#' x[-2]
#' x[2] <- paste0(transpose(x[2], 1), "~")
#' x
#'
#' # noteinfo class examples
#' x <- as_noteinfo(c("4-", "t8(", "t8)", "t8x", "8^", "16"))
#' x[2:4]
#' x[-1]
#' x[5:6] <- c("16^", "8")
#' x
#' x[x == "4-"]
#'
#' # music class examples
#' x <- as_music("c,~4 c,1 c'e_'g'4]*4")
#' x[1:3]
#' x[-c(1:2)]
#' x[3:6] <- "c'e'g'8"
#' x
`[.noteworthy` <- function(x, i){
  if(all(i == 0)) stop("Cannot have zero timesteps.", call. = FALSE)
  o <- octave_type(x)
  a <- accidental_type(x)
  format <- if(time_format(x) == "space-delimited time") "space" else "vector"
  x <- .Primitive("[")(.uncollapse(x), i)
  .asnw(x, o, a, format)
}

#' @name single-bracket
#' @export
`[.noteinfo` <- function(x, i){
  if(all(i == 0)) stop("Cannot have zero timesteps.", call. = FALSE)
  format <- if(time_format(x) == "space-delimited time") "space" else "vector"
  x <- .Primitive("[")(.uncollapse(x), i)
  .asni(x, format)
}

#' @name single-bracket
#' @export
`[.music` <- function(x, i){
  if(all(i == 0)) stop("Cannot have zero timesteps.", call. = FALSE)
  a <- accidental_type(x)
  key <- music_key(x)
  time <- music_time(x)
  tempo <- music_tempo(x)
  lyrics <- music_lyrics(x)
  s <- music_strings(x)
  if(!is.na(lyrics)) lyrics <- lyrics[i]
  if(!is.null(s)) s <- s[i]
  format <- if(time_format(x) == "space-delimited time") "space" else "vector"
  x <- .Primitive("[")(.uncollapse(x), i)
  notes <- gsub("^([a-grs_#~,']+).*", "\\1", x)
  info <- gsub("^[a-grs_#~,']+(.*)", "\\1", x)
  .asmusic(notes, info, s, lyrics, key, time, tempo, a, format)
}

#' @name single-bracket
#' @export
`[.lyrics` <- function(x, i){
  if(all(i == 0)) stop("Cannot have zero timesteps.", call. = FALSE)
  format <- if(time_format(x) == "space-delimited time") "space" else "vector"
  x <- .Primitive("[")(.uncollapse(x), i)
  .aslyrics(x, format)
}

#' @name single-bracket
#' @export
`[<-.noteworthy` <- function(x, i, value){
  o <- octave_type(x)
  a <- accidental_type(x)
  format <- if(time_format(x) == "space-delimited time") "space" else "vector"
  x <- .Primitive("[<-")(.uncollapse(x), i, value)
  .asnw(x, o, a, format)
}

#' @name single-bracket
#' @export
`[<-.noteinfo` <- function(x, i, value){
  format <- if(time_format(x) == "space-delimited time") "space" else "vector"
  x <- .Primitive("[<-")(.uncollapse(x), i, value)
  .asni(x, format)
}

#' @name single-bracket
#' @export
`[<-.music` <- function(x, i, value){
  a <- accidental_type(x)
  key <- music_key(x)
  time <- music_time(x)
  tempo <- music_tempo(x)
  lyrics <- music_lyrics(x)
  s <- music_strings(x)
  format <- if(time_format(x) == "space-delimited time") "space" else "vector"
  x <- .Primitive("[<-")(.uncollapse(x), i, value)
  notes <- gsub("^([a-grs_#~,']+).*", "\\1", x)
  info <- gsub("^[a-grs_#~,']+(.*)", "\\1", x)
  .asmusic(notes, info, s, lyrics, key, time, tempo, a, format)
}

#' @name single-bracket
#' @export
`[<-.lyrics` <- function(x, i, value){
  format <- if(time_format(x) == "space-delimited time") "space" else "vector"
  x <- .Primitive("[<-")(.uncollapse(x), i, value)
  .aslyrics(x, format)
}

#' Double bracket methods for tabr classes
#'
#' Double bracket indexing and assignment. See \code{\link{tabr-methods}} for
#' more details on methods for tabr classes.
#'
#' @param x object.
#' @param i index.
#' @param value values to assign at index.
#'
#' @name double-bracket
#' @seealso \code{\link{tabr-methods}}, \code{\link{note-metadata}}
#' @export
#'
#' @examples
#' # noteworthy class examples
#' x <- as_noteworthy("a, b, c ce_g")
#' x[[3]]
#' x[[2]] <- paste0(transpose(x[2], 1), "~")
#' x
#'
#' # noteinfo class examples
#' x <- as_noteinfo(c("4-", "t8(", "t8)", "t8x"))
#' x[[3]]
#' x[[3]] <- c("t8]")
#' x
#'
#' # music class examples
#' x <- as_music("c,~4 c,1 c'e_'g'4]*2")
#' x[[3]]
#' x[[3]] <- "c'e'g'8"
#' x
#' @name double-bracket
#' @export
`[[.noteworthy` <- function(x, i){
  .uncollapse(x)[[i]]
}

#' @name double-bracket
#' @export
`[[.noteinfo` <- function(x, i){
  .uncollapse(x)[[i]]
}

#' @name double-bracket
#' @export
`[[.music` <- function(x, i){
  .uncollapse(x)[[i]]
}

#' @name double-bracket
#' @export
`[[.lyrics` <- function(x, i){
  .uncollapse(x)[[i]]
}

#' @name double-bracket
#' @export
`[[<-.noteworthy` <- function(x, i, value){
  o <- octave_type(x)
  a <- accidental_type(x)
  format <- if(time_format(x) == "space-delimited time") "space" else "vector"
  x <- .Primitive("[[<-")(.uncollapse(x), i, value)
  .asnw(x, o, a, format)
}

#' @name double-bracket
#' @export
`[[<-.noteinfo` <- function(x, i, value){
  format <- if(time_format(x) == "space-delimited time") "space" else "vector"
  x <- .Primitive("[[<-")(.uncollapse(x), i, value)
  .asni(x, format)
}

#' @name double-bracket
#' @export
`[[<-.music` <- function(x, i, value){
  a <- accidental_type(x)
  key <- music_key(x)
  time <- music_time(x)
  tempo <- music_tempo(x)
  lyrics <- music_lyrics(x)
  s <- music_strings(x)
  format <- if(time_format(x) == "space-delimited time") "space" else "vector"
  x <- .Primitive("[[<-")(.uncollapse(x), i, value)
  notes <- gsub("^([a-grs_#~,']+).*", "\\1", x)
  info <- gsub("^[a-grs_#~,']+(.*)", "\\1", x)
  .asmusic(notes, info, s, lyrics, key, time, tempo, a, format)

}

#' @name double-bracket
#' @export
`[[<-.lyrics` <- function(x, i, value){
  format <- if(time_format(x) == "space-delimited time") "space" else "vector"
  x <- .Primitive("[[<-")(.uncollapse(x), i, value)
  .aslyrics(x, format)
}

#' Length for tabr classes
#'
#' Several methods are implemented for the classes \code{noteworthy},
#' \code{noteinfo}, and \code{music}. See \code{\link{tabr-methods}} for
#' more details on methods for tabr classes.
#'
#' @param x object.
#'
#' @name tabr-length
#' @seealso \code{\link{tabr-methods}}, \code{\link{note-metadata}}
#' @export
#'
#' @examples
#' # noteworthy class examples
#' x <- "a b c"
#' length(x)
#' length(as_noteworthy(x))
#' length(as_noteworthy("a b*2 c*2"))
#'
#' # noteinfo class examples
#' x <- "4- t8( t8)( t8) 4*2"
#' length(x)
#' length(as_noteinfo(x))
#'
#' # music class examples
#' x <- "c,~4 c,1 c'e_'g'4]*4"
#' length(x)
#' length(as_music(x))
length.noteworthy <- function(x){
  attr(x, "steps")
}

#' @name tabr-length
#' @export
length.noteinfo <- function(x){
  attr(x, "steps")
}

#' @name tabr-length
#' @export
length.music <- function(x){
  attr(x, "steps")
}

#' @name tabr-length
#' @export
length.lyrics <- function(x){
  attr(x, "steps")
}

#' Concatenate for tabr classes
#'
#' Several methods are implemented for the classes \code{noteworthy},
#' \code{noteinfo}, and \code{music}. See \code{\link{tabr-methods}} for
#' more details on methods for tabr classes.
#'
#' @param ... objects.
#'
#' @name tabr-c
#' @seealso \code{\link{tabr-methods}}, \code{\link{note-metadata}}
#' @export
#'
#' @examples
#' # noteworthy class examples
#' x <- "a b c"
#' c(x, x)
#' c(as_noteworthy(x), x)
#'
#' # noteinfo class examples
#' x <- "4- t8( t8)( t8) 4*2"
#' c(as_noteinfo(x), x)
#'
#' # music class examples
#' x <- "c,~4 c,1 c'e_'g'4]*2"
#' c(as_music(x), x)
#'
#' # phrase class examples
#' c(phrase(x), x)
c.noteworthy <- function(...){
  x <- list(...)
  cl <- sapply(lapply(x, class), "[", 1)
  x <- x[cl != "NULL"]
  cl <- cl[cl != "NULL"]
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

#' @name tabr-c
#' @export
c.noteinfo <- function(...){
  x <- list(...)
  cl <- sapply(lapply(x, class), "[", 1)
  x <- x[cl != "NULL"]
  cl <- cl[cl != "NULL"]
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

#' @name tabr-c
#' @export
c.music <- function(...){
  x <- list(...)
  cl <- sapply(lapply(x, class), "[", 1)
  x <- x[cl != "NULL"]
  cl <- cl[cl != "NULL"]
  if(any(!cl %in% c("music", "character")))
    stop("Cannot concatenate incompatible classes with 'music'.",
         call. = FALSE)
  idx <- which(cl == "character")
  if(length(idx)) x[idx] <- lapply(x[idx], as_music)
  key <- sapply(x, music_key)
  time <- sapply(x, music_time)
  tempo <- sapply(x, music_tempo)
  a <- sapply(x, accidental_type)
  format <- sapply(x, time_format)
  if(any(key != key[1])){
    warning("Key signature is inconsistent. Only the first is used.")
  }
  if(any(time != time[1])){
    warning("Time signature is inconsistent. Only the first is used.")
  }
  if(any(tempo != tempo[1])){
    warning("Tempo is inconsistent. Only the first is used.")
  }
  a <- if(!any(a == "flat")) "sharp" else "flat"
  format <- if(!any(format == "space-delimited time")) "vector" else "space"
  lyrics <- lapply(x, music_lyrics)
  lyrics_na <- sapply(lyrics, is.na)
  if(all(lyrics_na)){
    lyrics <- NA
  } else {
    idx <- which(lyrics_na)
    steps <- sapply(x[idx], length)
    lyrics[idx] <- lapply(steps, function(i) as_lyrics(rep(".", i), format))
    lyrics_space <- sapply(lyrics, is_space_time)
    if(format == "space" & any(!lyrics_space)){
      lyrics[!lyrics_space] <- lapply(lyrics[!lyrics_space], as_space_time)
    }
    if(format == "vector" & any(lyrics_space)){
      lyrics[lyrics_space] <- lapply(lyrics[lyrics_space], as_vector_time)
    }
    lyrics <- unlist(lyrics) %>% paste(collapse = " ")
    lyrics <- as_lyrics(lyrics, format)
  }
  s <- lapply(x, music_strings)
  s_null <- sapply(s, is.null)
  if(all(s_null)){
    s <- NULL
  } else {
    idx <- which(s_null)
    steps <- sapply(x[idx], length)
    s[idx] <- lapply(steps, function(i) rep("", i))
    s <- unlist(s)
  }
  x <- purrr::map_chr(x, ~paste(as.character(.x), collapse = " ")) %>%
    paste(collapse = " ") %>%
    .check_music_split()
  .asmusic(x$notes, x$info, s, lyrics, key[1], time[1], tempo[1], a, format)
}

#' @name tabr-c
#' @export
c.lyrics <- function(...){
  x <- list(...)
  cl <- sapply(lapply(x, class), "[", 1)
  x <- x[cl != "NULL"]
  cl <- cl[cl != "NULL"]
  if(any(!cl %in% c("lyrics", "character")))
    stop("Cannot concatenate incompatible classes with 'lyrics'.",
         call. = FALSE)
  idx <- which(cl == "character")
  if(length(idx)) x[idx] <- lapply(x[idx], as_lyrics)
  format <- sapply(x, time_format)
  format <- if(!any(format == "space-delimited time")) "vector" else "space"
  x <- purrr::map_chr(x, ~paste(as.character(.x), collapse = " ")) %>%
    paste(collapse = " ")
  .aslyrics(x, format)
}

#' @name tabr-c
#' @export
c.phrase <- function(...){
  x <- list(...)
  cl <- sapply(lapply(x, class), "[", 1)
  x <- x[cl != "NULL"]
  cl <- cl[cl != "NULL"]
  if(any(!cl %in% c("phrase", "character")))
    stop("Cannot concatenate incompatible classes with 'phrase'.",
         call. = FALSE)
  idx <- which(cl == "character")
  if(length(idx)) x[idx] <- lapply(x[idx], phrase)
  purrr::map_chr(x, ~as.character(.x)) %>% paste(collapse = " ") %>% as_phrase()
}

#' Repeat for tabr classes
#'
#' Several methods are implemented for the classes \code{noteworthy},
#' \code{noteinfo}, and \code{music}. See \code{\link{tabr-methods}} for
#' more details on methods for tabr classes.
#'
#' @param x object.
#' @param ... additional arguments. Not accepted for phrase objects.
#'
#' @name tabr-rep
#' @seealso \code{\link{tabr-methods}}, \code{\link{note-metadata}}
#' @export
#'
#' @examples
#' # noteworthy class examples
#' x <- "a b c"
#' rep(x, 2)
#' rep(as_noteworthy(x), 2)
#'
#' # noteinfo class examples
#' x <- "4x 4]*2 2"
#' rep(as_noteinfo(x), times = c(2, 1, 1, 2))
#'
#' # music class examples
#' x <- "c,~4 c,1 c'e_'g'4]"
#' rep(as_music(x), each = 2)
#'
#' # phrase class examples
#' rep(phrase(x), 2)
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

#' @name tabr-rep
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

#' @name tabr-rep
#' @export
rep.music <- function(x, ...){
  a <- accidental_type(x)
  format <- time_format(x)
  key <- music_key(x)
  time <- music_time(x)
  tempo <- music_tempo(x)
  lyrics <- music_lyrics(x)
  s <- music_strings(x)
  if(!is.na(lyrics)) lyrics <- rep(lyrics, ...)
  if(!is.null(s)) s <- rep(s, ...)
  format <- if(format == "space-delimited time") "space" else "vector"
  x <- .uncollapse(x)
  notes <- rep(.music_notes(x, format), ...)
  info <- rep(.music_info(x, format), ...)
  .asmusic(notes, info, s, lyrics, key, time, tempo, a, format)
}

#' @name tabr-rep
#' @export
rep.lyrics <- function(x, ...){
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
  .aslyrics(x, format)
}

#' @name tabr-rep
#' @export
rep.phrase <- function(x, ...){
  x <- as.character(x)
  y <- list(...)
  if(is.null(y$each) & is.null(y$times)){
    x <- rep(x, ...)
  } else if(!is.null(y$each)){
    stop("Cannot use `each` with a phrase.", call. = FALSE)
  } else {
    stop("Cannot use `times` with a phrase.", call. = FALSE)
  }
  if(!length(x)) stop("Cannot have zero timesteps.", call. = FALSE)
  as_phrase(paste0(x, collapse = " "))
}

#' Reverse for tabr classes
#'
#' Several methods are implemented for the classes \code{noteworthy},
#' \code{noteinfo}, and \code{music}. See \code{\link{tabr-methods}} for
#' more details on methods for tabr classes.
#'
#' @param x object.
#'
#' @name tabr-rev
#' @seealso \code{\link{tabr-methods}}, \code{\link{note-metadata}}
#' @export
#'
#' @examples
#' # noteworthy class examples
#' x <- "a b c"
#' rev(x)
#' rev(as_noteworthy(x))
#'
#' # noteinfo class examples
#' x <- "4x 4]*2 2"
#' rev(as_noteinfo(x))
#'
#' # music class examples
#' x <- "c,~4 c,1 c'e_'g'4]"
#' rev(as_music(x))
rev.noteworthy <- function(x){
  o <- octave_type(x)
  a <- accidental_type(x)
  format <- time_format(x)
  format <- if(format == "space-delimited time") "space" else "vector"
  x <- rev(.uncollapse(x))
  if(format == "space") x <- paste0(x, collapse = " ")
  .asnw(x, o, a, format)
}

#' @name tabr-rev
#' @export
rev.noteinfo <- function(x){
  format <- time_format(x)
  format <- if(format == "space-delimited time") "space" else "vector"
  x <- rev(.uncollapse(x))
  if(format == "space") x <- paste0(x, collapse = " ")
  .asni(x, format)
}

#' @name tabr-rev
#' @export
rev.music <- function(x){
  a <- accidental_type(x)
  key <- music_key(x)
  time <- music_time(x)
  tempo <- music_tempo(x)
  lyrics <- music_lyrics(x)
  s <- music_strings(x)
  if(!is.na(lyrics)) lyrics <- rev(lyrics)
  if(!is.null(s)) s <- rev(s)
  format <- time_format(x)
  format <- if(format == "space-delimited time") "space" else "vector"
  x <- rev(.uncollapse(x))
  notes <- .music_notes(x, format)
  info <- .music_info(x, format)
  .asmusic(notes, info, s, lyrics, key, time, tempo, a, format)
}

#' @name tabr-rev
#' @export
rev.lyrics <- function(x){
  format <- time_format(x)
  format <- if(format == "space-delimited time") "space" else "vector"
  x <- rev(.uncollapse(x))
  if(format == "space") x <- paste0(x, collapse = " ")
  .aslyrics(x, format)
}

#' Head and tail for tabr classes
#'
#' Several methods are implemented for the classes \code{noteworthy},
#' \code{noteinfo}, and \code{music}. See \code{\link{tabr-methods}} for
#' more details on methods for tabr classes.
#'
#' @param x object.
#' @param ... number of elements to return.
#'
#' @name tabr-head
#' @seealso \code{\link{tabr-methods}}, \code{\link{note-metadata}}
#' @importFrom utils head tail
#' @export
#'
#' @examples
#' # noteworthy class examples
#' x <- "a b c d e f g"
#' head(x, 2)
#' head(as_noteworthy(x), 2)
#' tail(as_noteworthy(x), 2)
#'
#' # noteinfo class examples
#' x <- "4x 4]*8 2 4"
#' head(as_noteinfo(x))
#' tail(as_noteinfo(x))
#'
#' # music class examples
#' x <- "c,~4 c,1 c'e_'g'4]"
#' head(as_music(x), 2)
#' tail(as_music(x), 2)
head.noteworthy <- function(x, ...){
  o <- octave_type(x)
  a <- accidental_type(x)
  format <- time_format(x)
  format <- if(format == "space-delimited time") "space" else "vector"
  x <- utils::head(.uncollapse(x), ...)
  if(format == "space") x <- paste0(x, collapse = " ")
  .asnw(x, o, a, format)
}

#' @name tabr-head
#' @export
head.noteinfo <- function(x, ...){
  format <- time_format(x)
  format <- if(format == "space-delimited time") "space" else "vector"
  x <- utils::head(.uncollapse(x), ...)
  if(format == "space") x <- paste0(x, collapse = " ")
  .asni(x, format)
}

#' @name tabr-head
#' @export
head.music <- function(x, ...){
  a <- accidental_type(x)
  key <- music_key(x)
  time <- music_time(x)
  tempo <- music_tempo(x)
  lyrics <- music_lyrics(x)
  s <- music_strings(x)
  if(!is.na(lyrics)) lyrics <- head(lyrics, ...)
  if(!is.null(s)) s <- head(s, ...)
  format <- time_format(x)
  format <- if(format == "space-delimited time") "space" else "vector"
  x <- utils::head(.uncollapse(x), ...)
  notes <- .music_notes(x, format)
  info <- .music_info(x, format)
  .asmusic(notes, info, s, lyrics, key, time, tempo, a, format)
}

#' @name tabr-head
#' @export
head.lyrics <- function(x, ...){
  format <- time_format(x)
  format <- if(format == "space-delimited time") "space" else "vector"
  x <- utils::head(.uncollapse(x), ...)
  if(format == "space") x <- paste0(x, collapse = " ")
  .aslyrics(x, format)
}

#' @name tabr-head
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

#' @name tabr-head
#' @export
tail.noteinfo <- function(x, ...){
  format <- time_format(x)
  format <- if(format == "space-delimited time") "space" else "vector"
  x <- utils::tail(.uncollapse(x), ...)
  if(format == "space") x <- paste0(x, collapse = " ")
  .asni(x, format)
}

#' @name tabr-head
#' @export
tail.music <- function(x, ...){
  a <- accidental_type(x)
  key <- music_key(x)
  time <- music_time(x)
  tempo <- music_tempo(x)
  lyrics <- music_lyrics(x)
  s <- music_strings(x)
  if(!is.na(lyrics)) lyrics <- tail(lyrics, ...)
  if(!is.null(s)) s <- tail(s, ...)
  format <- time_format(x)
  format <- if(format == "space-delimited time") "space" else "vector"
  x <- utils::tail(.uncollapse(x), ...)
  notes <- .music_notes(x, format)
  info <- .music_info(x, format)
  .asmusic(notes, info, s, lyrics, key, time, tempo, a, format)
}

#' @name tabr-head
#' @export
tail.lyrics <- function(x, ...){
  format <- time_format(x)
  format <- if(format == "space-delimited time") "space" else "vector"
  x <- utils::tail(.uncollapse(x), ...)
  if(format == "space") x <- paste0(x, collapse = " ")
  .aslyrics(x, format)
}

#' Logical operators for noteworthy class
#'
#' Logical operators for comparing two noteworthy class objects.
#'
#' Equality is assessed in the same manner as used for \code{\link{note_sort}}
#' when sorting pitches. What matters is the underlying semitone value
#' associated with each pitch, not the string notation such as flat vs. sharp
#' (see \code{\link{pitch_is_identical}}). When comparing chords, or a chord
#' vs. a single note, comparison favors the root. Comparison is made of the
#' respective lowest pitches, then proceeds to the next pitch if equal.
#'
#' For these operators, the objects on the left and right side of the operator
#' must both be \code{noteworthy} or an error is returned.
#'
#' The examples include a chord with its pitches entered out of pitch order.
#' This does not affect the results because pitches within chords are sorted
#' before note to note comparisons at each timestep are done between \code{e1}
#' and \code{e2}.
#'
#' @param e1 noteworthy string.
#' @param e2 noteworthy string.
#'
#' @return logical vector
#' @export
#' @name note-logic
#'
#' @examples
#' x <- as_noteworthy("f# a d'f#'a' d'f#'a'")
#' y <- as_noteworthy("g_ b f#'a'd' d'd''")
#' x == y
#' x != y
#' x < y
#' x > y
#' x <= y
#' x >= y
`==.noteworthy` <- function(e1, e2){
  rowSums(.logic_diff(e1, e2)) == 0
}

#' @export
#' @rdname note-logic
`!=.noteworthy` <- function(e1, e2){
  rowSums(.logic_diff(e1, e2)) != 0
}

#' @export
#' @rdname note-logic
`<.noteworthy` <- function(e1, e2){
  .logic_comp(e1, e2) < 0
}

#' @export
#' @rdname note-logic
`<=.noteworthy` <- function(e1, e2){
  .logic_comp(e1, e2) <= 0
}

#' @export
#' @rdname note-logic
`>.noteworthy` <- function(e1, e2){
  .logic_comp(e1, e2) > 0
}

#' @export
#' @rdname note-logic
`>=.noteworthy` <- function(e1, e2){
  .logic_comp(e1, e2) >= 0
}

.logic_diff <- function(x, y){
  if(!is_noteworthy(x) | !is_noteworthy(y))
    stop("Left and right hand side must both be `noteworthy` class.",
         call. = FALSE)
  .logic_prep(x) - .logic_prep(y)
}

.logic_prep <- function(x){
  x <- .uncollapse(x)
  s <- lapply(chord_semitones(x), sort)
  n <- max(sapply(s, length))
  s <- purrr::map(s, ~{
    x <- rep(NA_integer_, n)
    x[seq_along(.x)] <- .x
    x[is.na(x)] <- utils::tail(.x, 1)
    x
  })
  unname(t(as.data.frame(s)))
}
.logic_comp <- function(x, y){
  apply(.logic_diff(x, y), 1, .logic_first)
}

.logic_first <- function(x){
  i <- which(x != 0)
  if(length(i)) x[i[1]] else 0L
}
