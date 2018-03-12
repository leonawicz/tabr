#' Tied notes
#'
#' Tie notes efficiently.
#'
#' This is useful for bar chords.
#'
#' @param x character.
#'
#' @return a character string.
#' @export
#'
#' @examples
#' tie("e,b,egbe'")
tie <- function(x){
  paste0(.split_chord(x), "~", collapse = "")
}

#' Create rests
#'
#' Create multiple rests efficiently with a simple wrapper around \code{rep} using the \code{times} argument.
#'
#' @param x integer, duration.
#' @param n integer, number of repetitions.
#'
#' @return a character string.
#' @export
#'
#' @examples
#' rest(c(1, 8), c(1, 4))
rest <- function(x, n = 1){
  paste0(rep(paste0("r", x), times = n), collapse = " ")
}

#' Add text to music staff
#'
#' Annotate a music staff, vertically aligned above or below the music staff at a specific note/time.
#'
#' This function binds text annotation in LilyPond syntax to a note's associated \code{info} entry.
#' Technically, the syntax is a hybrid form, but is later updated safely and unambiguously to LilyPond syntax with respect to the rest of the note info substring when it is fed to \code{phrase} for musical phrase assembly.
#'
#' @param x character.
#' @param text character.
#' @param position character, top or bottom.
#'
#' @return a character string.
#' @export
#'
#' @examples
#' notate("8", "Solo")
#' phrase("c'~ c' d' e'", glue(notate(8, "First solo"), "8 8 4."), "5 5 5 5")
notate <- function(x, text, position = "top"){
  pos <- switch(position, top = "^", bottom = "_")
  paste0(x, ";", pos, "\"", gsub(" ", "_", text), "\"", collapse = " ")
}

#' Append and duplicate
#'
#' Helper functions for appending or pasting musical phrases and other strings together as well as repetition.
#' The functions \code{glue} and \code{dup} are based on base functions \code{paste} and\code{rep}, respectively, but are tailored for efficiency in creating musical phrases.
#' These functions respect and retain the phrase class when applied to phrases.
#' Combining a phrase with a non-phrase string will assume compatibility and result in a new phrase object.
#'
#' This is especially useful for repeated instances. This function applies to general slur notation as well.
#' Multiple input formats are allowed. Total number of note durations must be even because all slurs require start and stop points.
#'
#' @param ... character, phrase or non-phrase string.
#' @param x character, phrase or non-phrase string.
#' @param n integer, number of repetitions.
#' @name append_phrases
#'
#' @return phrase on non-phrase character string.
#'
#' @examples
#' glue(8, "16-", "8^")
#' dup(1, 2)
#' x <- phrase("c ec'g' ec'g'", "4 4 2", "5 432 432")
#' y <- phrase("a", 1, 5)
#' glue(x, y)
#' glue(x, dup(y, 2))
#' glue(x, "r1") # add a simple rest instance
#' class(glue(x, y))
#' class(dup(y, 2))
#' class(glue(x, "r1"))
#' class(dup("r1", 2))
#' class(glue("r1", "r4"))
NULL

#' @export
#' @rdname append_phrases
glue <- function(...){
  x <- list(...)
  is_phrase <- any(unlist(purrr::map(x, class)) == "phrase")
  x <- paste(unlist(x), collapse = " ")
  if(is_phrase) class(x) <- unique(c("phrase", class(x)))
  x
}

#' @export
#' @rdname append_phrases
dup <- function(x, n = 1){
  if(n == 0) return(x)
  is_phrase <- "phrase" %in% class(x)
  x <- paste(rep(x, n), collapse = " ")
  if(is_phrase) class(x) <- unique(c("phrase", class(x)))
  x
}

#' Hammer ons and pull offs
#'
#' Helper function for generating hammer on and pull off syntax.
#'
#' This is especially useful for repeated instances. This function applies to general slur notation as well.
#' Multiple input formats are allowed. Total number of note durations must be even because all slurs require start and stop points.
#'
#' @param ... character, note durations. Numeric is allowed for lists of single inputs. See examples.
#'
#' @return character.
#' @export
#'
#' @examples
#' hp(16, 16)
#' hp("16 16")
#' hp("16 8 16", "8 16 8")
hp <- function(...){
  x <- unlist(purrr::map(list(...), ~paste0(strsplit(as.character(paste0(.x, collapse = " ")), " ")[[1]])))
  if(length(x) %% 2 == 1) stop("Even number of arguments required.")
  idx <- seq_along(x) %% 2 == 0
  x[idx == TRUE] <- paste0(x[idx == TRUE], ")")
  x[idx == FALSE] <- paste0(x[idx == FALSE], "(")
  paste(x, collapse = " ")
}

#' Tuplets
#'
#' Helper function for generating tuplet syntax.
#'
#' This is an initial version.
#'
#' @param notes character, notes.
#' @param n integer, tuplet beat, e.g., 8 for 8th note tuplet.
#'
#' @return character.
#' @export
#'
#' @examples
#' tuplet("c c# d", 8)
tuplet <- function(notes, n){
  paste("\\tuplet 3/2", n, "{", notes, "}")
}

barcheck <- function(phrase, bpm = 8){
  x <- unlist(purrr::map(strsplit(phrase, ">")[[1]], ~strsplit(.x, "[ |^]")), recursive = FALSE)
  y <- purrr::map_dbl(2:length(x), ~({
    a <- x[[.x]][1]
    n <- length(grep("\\.", a))
    if(n == 0) return(1 / as.numeric(a))
    a <- 1 / as.numeric(a)
    sum(c(a, a + 0.5 * a, a + 0.25 * a)[1:n])
  }))
  bars <- which(cumsum(y) %% 1 == 0)
  x[1] <- strsplit(x[1], "\\\\")[[1]][2]
}
