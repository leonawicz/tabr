#' Create a musical phrase
#'
#' Create a musical phrase from character strings that define notes, note metadata, and optionally explicit strings fretted. The latter can be used to ensure proper tablature layout.
#' Notes separated in time are separated in the \code{notes} string by spaces. Sharps and flats are indicated by appending \code{#} and \code{_}, respectively, e.g. \code{f#} or \code{g_}.
#'
#' Specifying notes that are one or multiple octaves below or above the middle can be done by appending one or multiple commas or single quote tick marks, respectively, e.g. \code{c,} or \code{c''}.
#' But this is not necessary. Instead, you can use octave numbering. This may easier to read, generally more familiar, potentially requires less typing, can still be omitted completely
#' for the middle octave (no need to type c3, d3, ...), and is automatically converted for you by \code{phrase} to the tick mark format interpreted by LilyPond.
#' That said, using the raised and lowered tick mark approach can be surprisingly easier to read for chords, which have no spaces between notes, especially six-string chords,
#' given that the tick marks help break up the notes in the chord visually much more so than integers do. See examples.
#'
#' The function \code{p} is a convenient shorthand wrapper for \code{phrase}.
#'
#' Tied notes indicated by \code{~} are part of the \code{note} notation and not part of the \code{info} notation, e.g. \code{c''~}.
#'
#' Notes be comprise chords. These are bound tightly rather than space-delimited, as they are not separated in time.
#' For example, a C chord could be given as \code{ceg} and in the case of tied notes would be \code{c~e~g~}.
#'
#' Other information about a note is indicated with the \code{info} string.
#' The most pertinent information, minimally required, is the note duration. A string of space-delimited \code{notes} will always be accompanied by a space-delimited string of an equal number of integer durations.
#' Durations are powers of 2: 1, 2, 4, 8, 16, 32, 64. They represent the fraction of a measure, e.g., 2 means 1/2 of a measure and 8 refers to an eighth note.
#' Dotted notes are indicated by adding \code{.} immediately after the integer, e.g., \code{2.} or \code{2..}.
#' Any other note metadata is appended to these durations. See examples.
#'
#' Opening and closing slurs (or hammer ons and pull offs) are indicated with opening and closing parentheses, slides with \code{-}, and simple bends with \code{^}.
#' Text annotations aligned vertically with a note in time on the staff is done by appending the text to the note info entry itself. See \code{\link{notate}}.
#' More complete and powerful features will be added later.
#'
#' @param notes character, notes \code{a} through \code{g}. See details.
#' @param info character, metadata pertaining to the \code{notes }. See details.
#' @param string character, optional string that specifies which guitar strings to play for each specific note.
#' @param bar logical, insert a bar check at the end of the phrase.
#' @param ... arguments passed to \code{phrase}.
#'
#' @return a phrase.
#' @name phrase
#' @export
#'
#' @examples
#' phrase("c ec'g' ec'g'", "4 4 2") # no explicit string specification (not recommended)
#' phrase("c ec4g4 ec4g4", "4 4 2") # same as above
#' phrase("c b, c", "4. 8( 8)", "5 5 5") # direction implies hammer on
#' phrase("b2 c d", "4( 4)- 2", "5 5 5") # hammer and slide
#' phrase("c ec'g' ec'g'", "1 1 1", "5 432 432")
NULL

# nolint start

#' @export
#' @rdname phrase
phrase <- function(notes, info, string = NULL, bar = FALSE){
  notes <- strsplit(notes, " ")[[1]]
  notes <- .octavesub(notes)
  info <- strsplit(as.character(info), " ")[[1]]
  notes <- purrr::map_chr(notes, ~.tabsub(.x))
  info <- purrr::map_chr(info, ~.tabsub(.x))
  bend <- which(purrr::map_int(info, ~length(grep("\\^", strsplit(.x, ";")[[1]][1]))) == 1)
  dead <- which(purrr::map_int(info, ~length(grep("xDEADNOTEx", strsplit(.x, ";")[[1]][1]))) == 1)
  if(length(bend)) info[bend] <- gsub(";\\^", ";", info[bend])
  if(length(dead)) info[dead] <- gsub("xDEADNOTEx", "", info[dead])
  info <- gsub(";", "", info)
  .bend <- "\\bendAfter #+6"
  s <- !is.null(string)
  if(s) string <- .strsub(string)
  notes <- purrr::map_chr(
    seq_along(notes),
    ~paste0("<", paste0(.split_chord(notes[.x]),
                        if(s && notes[.x] != "r")
                          paste0("\\", .split_chord(string[.x], TRUE)), collapse = " "), ">"))
  notes <- gsub("<r>", "r", notes)
  x <- paste0(notes, info)
  if(length(bend)) x[bend] <- paste0(x[bend], .bend)
  if(length(dead)) x[dead] <- paste("\\deadNote", x[dead])
  x <- gsub("\\\\x", "", x)
  x <- paste(x, collapse = " ")
  if(bar) x <- paste(x, "|")
  x <- gsub("\\| \\|", "\\|", x)
  class(x) <- c("phrase", class(x))
  x
}

# nolint end

#' @export
#' @rdname phrase
p <- function(...) phrase(...)

#' @export
print.phrase <- function(x, ...) cat(x)

#' Create a volta/repeat phrase
#'
#' Create a repeat section in LilyPond readable format.
#'
#' This function takes a musical phrase or even a basic string and converts it into a volta phrase for the LilyPond volta engraver.
#'
#' Note that basic strings should still be interpretable as a valid musical phrase by LilyPond.
#' For example, a one-measure rest, \code{"r1"} does not need to be wrapped in a phrase class to work, nor would any other string explicitly written out in valid LilyPond syntax.
#' As always, see the LilyPond documentation if you are not familiar with LilyPond syntax.
#'
#' @param phrase a phrase or basic string to be repeated.
#' @param n integer, number of repeats of \code{phrase} (one less than the total number of plays).
#' @param endings list of phrases or basic strings, alternate endings.
#' @param silent if \code{TRUE}, no text will be printed above the staff to indicate the number of repeats when greater than one and no \code{endings = NULL}. This is useful when another staff already displays the text.
#'
#' @return a phrase.
#' @export
#'
#' @examples
#' x <- phrase("c ec'g' ec'g'", "4 4 2", "5 432 432")
#' e1 <- phrase("a", 1, 5)
#' e2 <- phrase("b", 1, 5)
#' volta(x) # one repeat or 2 plays
#' pct(x) # as above, simple percent repeat notation
#' volta(x, 1, list(e1, e2)) # one repeat with alternate ending
#' volta(x, 4, list(e1, e2)) # multiple repeats but with only one alternate ending
volta <- function(phrase, n = 1, endings = NULL, silent = FALSE){
  if(n > 1 & is.null(endings) & !silent){
    phrase <- strsplit(phrase, " ")[[1]]
    idx <- grep(">", phrase)
    if(length(idx)){
      idx <- min(idx)
      phrase[idx] <- paste0(phrase[idx], paste("^\"Repeat", n, "times.\""))
    }
    phrase <- paste0(phrase, collapse = " ")
  }
  x <- paste("\\repeat volta", n, "{", paste(phrase, collapse = " "), "| }\n")
  if(!is.null(endings)){
    x <- paste0(x, "\\alternative {\n", paste("  {", endings, "| }\n", collapse = ""), "}")
  }
  x <- gsub("\\| \\|", "\\|", x)
  class(x) <- c("phrase", class(x))
  x
}

#' @export
#' @rdname volta
pct <- function(phrase, n = 1){
  x <- paste("\\repeat percent", n + 1, "{", paste(phrase, collapse = " "), " }\n")
  class(x) <- c("phrase", class(x))
  x
}
