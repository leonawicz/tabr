#' Create a musical phrase
#'
#' Create a musical phrase from character strings that define notes, note
#' metadata, and optionally explicit strings fretted. The latter can be used to
#' ensure proper tablature layout.
#'
#' Meeting all of the requirements for a string of notes to be valid
#' \code{tabr} syntax is referred to as \emph{noteworthy}. Noteworthy strings
#' are referred to throughout the documentation.
#' Such requirements are outlined below.
#'
#' Noteworthy strings use space-delimited time. This means that notes and
#' chords separated in time are separated in the \code{notes} string by spaces.
#' This is by far the most common usage. However, many functions in
#' \code{tabr}, including \code{phrase},
#' allow a \code{notes} or similar first function argument to be provided in
#' vector form where each vector element is a single note or chord (single
#' point in time).
#' Internally, functions like \code{phrase} will manipulate these forms back
#' and forth as needed. Having both input options provides useful flexibility
#' for music programming in \code{tabr} in general.
#' The pipe operator can also be leveraged to chain several functions together.
#'
#' Sharps and flats are indicated by appending \code{#} and \code{_},
#' respectively, e.g. \code{f#} or \code{g_}.
#'
#' Specifying notes that are one or multiple octaves below or above the third
#' octave can be done by appending one or multiple commas or single quote
#' tick marks, respectively, e.g. \code{c,} or \code{c''}.
#' But this is not necessary. Instead, you can use octave numbering. This may
#' be easier to read, generally more familiar,
#' potentially requires less typing, can still be omitted completely
#' for the third octave (no need to type c3, d3, ...), and is automatically
#' converted for you by \code{phrase} to the tick mark format interpreted by
#' LilyPond.
#' That said, using the raised and lowered tick mark approach can be
#' surprisingly easier to read for chords, which have no spaces between notes,
#' especially six-string chords,
#' given that the tick marks help break up the notes in the chord visually much
#' more so than integers do. See examples.
#'
#' The function \code{p} is a convenient shorthand wrapper for \code{phrase}.
#'
#' Tied notes indicated by \code{~} are part of the \code{note} notation and
#' not part of the \code{info} notation, e.g. \code{c''~}.
#'
#' Notes can comprise chords. These are bound tightly rather than
#' space-delimited, as they are not separated in time.
#' For example, a C chord could be given as \code{ceg} and in the case of tied
#' notes would be \code{c~e~g~}.
#'
#' Other information about a note is indicated with the \code{info} string.
#' The most pertinent information, minimally required, is the note duration.
#' A string of space-delimited \code{notes} will always be accompanied by a
#' space-delimited string of an equal number of integer durations.
#' Durations are powers of 2: 1, 2, 4, 8, 16, 32, 64. They represent the
#' fraction of a measure, e.g., 2 means 1/2 of a measure and 8 refers to an
#' eighth note.
#' Dotted notes are indicated by adding \code{.} immediately after the integer,
#' e.g., \code{2.} or \code{2..}.
#' Any other note metadata is appended to these durations. See examples.
#'
#' Opening and closing slurs (or hammer ons and pull offs) are indicated with
#' opening and closing parentheses, slides with \code{-}, and simple bends with
#' \code{^}.
#' Text annotations aligned vertically with a note in time on the staff is done
#' by appending the text to the note info entry itself.
#' See \code{\link{notate}}.
#' For more details and example, see the package vignettes.
#'
#' @param notes character, notes \code{a} through \code{g}, comprising a
#' noteworthy string. \code{notes}. See details.
#' @param info character, metadata pertaining to the \code{notes }. See details.
#' @param string character, optional string that specifies which guitar strings
#' to play for each specific note.
#' @param bar logical, insert a bar check at the end of the phrase.
#'
#' @return a phrase.
#' @name phrase
#' @export
#'
#' @examples
#' phrase("c ec'g' ec'g'", "4 4 2") # no string numbers (not recommended)
#' phrase("c ec4g4 ec4g4", "4 4 2") # same as above
#' phrase("c b, c", "4. 8( 8)", "5 5 5") # direction implies hammer on
#' phrase("b2 c d", "4( 4)- 2", "5 5 5") # hammer and slide
#'
#' phrase("c ec'g' ec'g'", "1 1 1", "5 432 432")
#' p("c ec'g' ec'g'", "1 1 1", "5 432 432") # same as above
NULL

#' @export
#' @rdname phrase
phrase <- function(notes, info, string = NULL, bar = FALSE){
  .check_noteworthy(notes)
  if(length(notes) > 1) notes <- paste(notes, collapse = " ")
  .check_phrase_input(info, "info")
  if(length(string) == 1 && is.na(string)) string <- NULL
  if(!is.null(string)) .check_phrase_input(string, "string")
  notes <- (strsplit(notes, " ")[[1]] %>%
              purrr::map_chr(.star_expand) %>%
    paste0(collapse = " ") %>%
      strsplit(" "))[[1]]
  notes <- .octave_to_tick(notes)
  info <- (strsplit(as.character(info), " ")[[1]] %>%
             purrr::map_chr(.star_expand) %>%
             paste0(collapse = " ") %>%
             strsplit(" "))[[1]]
  notes <- purrr::map_chr(notes, .tabsub)
  info <- purrr::map_chr(info, .tabsub)
  bend <- which(purrr::map_int(info, ~{
    length(grep("\\^", strsplit(.x, ";")[[1]][1]))
  }) == 1)
  dead <- which(purrr::map_int(info, ~{
    length(grep("xDEADNOTEx", strsplit(.x, ";")[[1]][1]))
  }) == 1)
  if(length(bend)) info[bend] <- gsub(";\\^", ";", info[bend])
  if(length(dead)) info[dead] <- gsub("xDEADNOTEx", "", info[dead])
  info <- gsub(";", "", info)
  .bend <- "\\bendAfter #+6"
  s <- !is.null(string)
  if(s && is.numeric(string))
    string <- paste0(rep(string, length(notes)), collapse = " ")
  if(s) string <- .strsub(string)
  notes <- purrr::map_chr(
    seq_along(notes),
    ~paste0("<", paste0(
      .split_chord(notes[.x], abb = TRUE),
      if(s && notes[.x] != "r" && notes[.x] != "s")
        paste0("\\", .split_chord(string[.x], TRUE)), collapse = " "), ">"))
  notes <- gsub("<s>", "s", gsub("<r>", "r", notes))
  x <- paste0(notes, info)
  if(length(bend)) x[bend] <- paste0(x[bend], .bend)
  if(length(dead)) x[dead] <- paste("\\deadNote", x[dead])
  x <- gsub("\\\\x", "", x)
  x <- paste(x, collapse = " ")
  if(bar) x <- paste(x, "|")
  x <- gsub("\\| \\|", "\\|", x)
  as_phrase(x)
}

.check_phrase_input <- function(x, y){
  if(length(x) > 1) stop(paste0("`", y, "` must be length one."), call. = FALSE)
}

#' @export
#' @rdname phrase
p <- phrase

#' @export
print.phrase <- function(x, ...){
  x <- gsub("\n\n", "\n", x)
  col1 <- crayon::make_style("gray50")
  notes <- crayon::make_style("dodgerblue")$bold
  info <- crayon::make_style("orange2")
  strings <- crayon::make_style("firebrick")
  octaves <- crayon::make_style("dodgerblue")
  pat <- "(<| )([a-girs]+)([,'\\d]+|)(~|)(\\\\\\d|)( |>)"
  repl <- paste0("\\1", notes("\\2"), octaves("\\3"), "\\4", strings("\\5"),
                 "\\6\\7")
  x <- gsub(pat, repl, x)
  x <- gsub(pat, repl, x)
  x <- gsub(">(\\d)([\\.\\(\\)]+)( <|\\^|)", paste0(">", info("\\1\\2"), "\\3"), x)
  x <- gsub(">(\\d|)(\\.+|)(\\\\[a-zA-Z]+|)", paste0(">", info("\\1\\2\\3")), x)
  x <- gsub("(\\\\deadNote) ", info("\\1 "), x)
  x <- gsub("(\\^\\\\bendAfter #\\+6)", info("\\1"), x)
  x <- gsub("(~)", info("\\1"), x)
  x <- gsub("(r|s)(\\d+)", paste0(notes("\\1"), info("\\2")), x)
  cat(col1("<"),
      col1$bold("Musical phrase"), col1(">"), "\n", col1(x), sep = "")
}

#' Phrase validation and coercion
#'
#' These helper functions add some validation checks for phrase and candidate
#' phrase objects.
#'
#' Use these functions with some caution. They are not intended for strictness
#' and perfection.
#' \code{phrasey} checks whether an object is weakly phrase-like and returns
#' \code{TRUE} or \code{FALSE}.
#' It can be used to safeguard against the most obvious cases of \code{phrase}
#' not containing valid phrase syntax when programming.
#' However, it may also be limiting. Use wear sensible.
#'
#' \code{as_phrase} coerces an object to a phrase object if possible.
#' This function performs an internal \code{phrasey} check.
#'
#' \code{notify} attempts to decompose a phrase object back to its original
#' input vectors consisting of notes, note info, and optionally, instrument
#' string numbering.
#' If successful, it returns a tibble data frame with columns: \code{notes},
#' \code{info}, \code{string}.
#'
#' Unless decomposing very simple phrases, this function is likely to reveal
#' limitations.
#' Complex phrase objects constructed originally with \code{phrase} can be
#' challenging to deconstruct in a one to one manner.
#' Information may be lost, garbled, or the function may fail.
#' For example, this function is not advanced enough to unravel repeat notation
#' or handle arbitrary text notations attached to notes.
#'
#'  \code{notable} returns \code{TRUE} or \code{FALSE} regarding whether a
#'  phrase can be converted back to character string inputs,
#'  not necessarily with complete correctness, but without simple failure.
#'  It checks for phrasiness. Then it tries to call \code{notify} and returns
#'  \code{FALSE} gracefully if that call throws an exception.
#'
#' @param phrase phrase object or character string (candidate phrase).
#' @param collapse logical, collapse result into a single string ready for
#' phrase construction.
#'
#' @return see details for each function's purpose and return value.
#' @export
#' @name phrase-checks
#'
#' @examples
#' # Create a list of phrase objects
#' p1 <- phrase("c ec'g' ec'g'", "4 4 2") # no string numbers (not recommended)
#' p2 <- phrase("c ec4g4 ec4g4", "4 4 2") # same as above
#' p3 <- phrase("c b, c", "4. 8( 8)", "5 5 5") # direction implies hammer on
#' p4 <- phrase("b2 c d", "4( 4)- 2", "5 5 5") # hammer and slide
#' p5 <- phrase("c ec'g'~ ec'g'", 1, "5 432 432") # tied chord
#' x <- list(p1, p2, p3, p4, p5)
#'
#' # Check if phrases and strings are phrasey
#' sapply(x, phrasey)
#' sapply(as.character(x), phrasey, USE.NAMES = FALSE)
#'
#' # Coerce character string representation to phrase and compare with original
#' y <- lapply(as.character(x), as_phrase)
#' identical(x, y)
#'
#' # Check if notable
#' sapply(x, notable)
#' notable(p("a b c", 1))
#' notable("a b x") # note: not constructible as a phrase in the first place
#'
#' # Notify phrases
#' d <- do.call(rbind, lapply(x, notify))
#' d
#'
#' # Wrappers around notify extract components, default to collapsed strings
#' phrase_notes(p5)
#' phrase_info(p5)
#' phrase_strings(p5)
#'
#' # If phrase decomposition works well, coercion is one to one
#' x2 <- lapply(x,
#'   function(x) p(phrase_notes(x), phrase_info(x), phrase_strings(x))
#' )
#' identical(x, x2)
as_phrase <- function(phrase){
  x <- phrase
  if(inherits(x, "phrase")) return(x)
  if(!inherits(x, "character"))
    stop(paste("Cannot coerce", class(x)[1], "to phrase."), call. = FALSE)
  if(!phrasey(x)) stop("`x` is not phrasey.", call. = FALSE)
  class(x) <- unique(c("phrase", class(x)))
  x
}

#' @export
#' @rdname phrase-checks
phrasey <- function(phrase){
  if(!inherits(phrase, "phrase") & !inherits(phrase, "character")) return(FALSE)
  i1 <- sum(attr(gregexpr("<", phrase)[[1]], "match.length"))
  if(i1 < 1){
    if(gsub(" |(r|s)\\d+(\\.|)(\\.|)", "", phrase) == ""){
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
  i2 <- sum(attr(gregexpr(">", phrase)[[1]], "match.length"))
  if(i1 != i2) return(FALSE)
  TRUE
}

#' @export
#' @rdname phrase-checks
notify <- function(phrase){
  if(!phrasey(phrase)) stop("`phrase` is not phrasey.", call. = FALSE)
  x <- .tag_rests(phrase)
  x <- strsplit(x, " <")[[1]]
  x <- gsub("\\\\glissando", "-", x)
  x <- gsub("is", "#", x)
  x <- gsub("(^|<)([a|e])s", "\\2_", x)
  x <- gsub(" ([a|e])s", " \\1_", x)
  x <- gsub("es", "_", x)
  x <- gsub(" ", "", x)
  x <- gsub("^<", "", x)
  x <- strsplit(x, ">")
  notes <- sapply(x, "[[", 1)
  info <- sapply(x, "[[", 2)

  pat <- "\\\\\\d+"
  y <- gregexpr(pat, notes)

  f <- function(i){
    start <- y[[i]] + 1
    end <- start + attributes(y[[i]])$match.length - 2
    y <- sapply(seq_along(start),
                function(x) substr(notes[i], start[x], end[x]))
    y <- paste(y, collapse = "")
    y[y == ""] <- NA
    y
  }

  string <- sapply(seq_along(y), f)
  notes <- gsub(pat, "", notes)
  tibble::tibble(notes = notes, info = info, string = string)
}

.tag_rests <- function(x){
  gsub("(| )(r|s)(\\d)", "\\1<\\2>\\3", x)
}

#' @export
#' @rdname phrase-checks
phrase_notes <- function(phrase, collapse = TRUE){
  x <- notify(phrase)$notes
  if(collapse) x <- paste(x, collapse = " ")
  .asnw(x)
}

#' @export
#' @rdname phrase-checks
phrase_info <- function(phrase, collapse = TRUE){
  x <- notify(phrase)$info
  if(collapse) x <- paste(x, collapse = " ")
  x
}

#' @export
#' @rdname phrase-checks
phrase_strings <- function(phrase, collapse = TRUE){
  x <- notify(phrase)$string
  if(collapse){
    x <- if(any(is.na(x))) as.character(NA) else paste(x, collapse = " ")
  }
  x
}

#' @export
#' @rdname phrase-checks
notable <- function(phrase){
  if(!phrasey(phrase)) return(FALSE)
  tryCatch(notify(phrase), error = function(e) FALSE)
  TRUE
}
