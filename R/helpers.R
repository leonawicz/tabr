#' Tied notes
#'
#' Tie notes efficiently.
#'
#' This function is useful for bar chords.
#'
#' @param x character, a single chord.
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
#' This function gives control over tuplet construction. The default arguments \code{a = 3} and \code{b = 2} generates a triplet where three triplet notes, each lasting for two thirds of a beat, take up two beats.
#' \code{n} is used to describe the beat duration with the same fraction-of-measure denominator notation used for notes in \code{tabr} phrases, e.g., 16th note triplet, 8th note triplet, etc.
#'
#' If you provide a note sequence for multiple tuplets in a row of the same type, they will be connected automatically. It is not necessary to call \code{tuplet} each time when the pattern is constant.
#' If you provide a complete phrase object, it will simply be wrapped in the tuplet tag, so take care to ensure the phrase contents make sense as part of a tuplet.
#'
#' @param x phrase object or character string of notes.
#' @param n integer, duration of each tuplet note, e.g., 8 for 8th note tuplet.
#' @param string, character, optional string that specifies which guitar strings to play for each specific note.
#' @param a integer, notes per tuplet.
#' @param b integer, beats per tuplet.
#'
#' @return character.
#' @export
#'
#' @examples
#' tuplet("c c# d", 8)
#' triplet("c c# d", 8)
#' tuplet("c c# d c c# d", 4, a = 6, b = 4)
#'
#' p1 <- phrase("c c# d", "8] 8( 8)", "5*3")
#' tuplet(p1, 8)
tuplet <- function(x, n, string = NULL, a = 3, b = 2){
  if("phrase" %in% class(x)){
    x <- paste0("\\tuplet ", a, "/", b, " ", n / b, " { ", x, " }")
    class(x) <- c("phrase", class(x))
    return(x)
  } else {
    notes <- x
  }
  notes <- .octavesub(.notesub(notes))
  notes <- strsplit(notes, " ")[[1]]
  s <- !is.null(string)
  if(s) string <- .strsub(string)
  notes <- purrr::map_chr(
    seq_along(notes),
    ~paste0("<", paste0(.split_chord(notes[.x]),
                        if(s && notes[.x] != "r" && notes[.x] != "s")
                          paste0("\\", .split_chord(string[.x], TRUE)), collapse = " "), ">"))
  notes[1] <- paste0(notes[1], n)
  notes <- paste0(notes, collapse = " ")
  notes <- gsub("<r>", "r", notes)
  notes <- gsub("<s>", "s", notes)
  x <- paste0("\\tuplet ", a, "/", b, " ", n / b, " { ", notes, " }")
  class(x) <- c("phrase", class(x))
  x
}

#' @export
#' @rdname tuplet
triplet <- function(x, n, string = NULL) tuplet(x = x, n = n, string = string, a = 3, b = 2)
