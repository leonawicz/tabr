#' Create a musical phrase
#'
#' Create a musical phrase from character strings that define notes, note
#' metadata, and optionally explicit strings fretted. The latter can be used to
#' ensure proper tablature layout.
#'
#' A phrase object combines a valid string of notes with a corresponding valid
#' string of note info. The only required note info is time, but other
#' information can be included as well. You do not need to input an existing
#' \code{noteworthy} class object and \code{noteinfo} class object, but both
#' inputs must be valid and thus coercible to these classes. This is similar to
#' how the \code{music} class works. The difference with phrase objects is that
#' they are used to create LilyPond syntax analogous to what a music object
#' contains.
#'
#' See the help documentation on \code{noteworthy}, \code{noteinfo}, and
#' \code{music} classes for an understanding of the input data structures.
#' The function \code{p} is a convenient shorthand wrapper for \code{phrase}.
#'
#' @param notes,info noteworthy and note info strings. When \code{info = NULL},
#' it is assumed that \code{notes} refers to a music object or string formatted
#' as such.
#' @param string space-delimited character string or vector (or integer vector
#' if simple string numbers). This is an optional argument that specifies which
#' instrument strings to play for each specific timestep. Otherwise \code{NULL}.
#' @param bar logical, insert a bar check at the end of the phrase.
#'
#' @return a phrase.
#' @export
#' @seealso \code{\link{valid-notes}}, \code{\link{valid-noteinfo}},
#' \code{\link{music}}
#'
#' @examples
#' phrase("c ec'g' ec'g'", "4 4 2") # no string arg (not recommended for tabs)
#' phrase("c ec4g4 ec4g4", "4 4 2") # same as above
#' phrase("c b, c", "4. 8( 8)", "5 5 5") # direction implies hammer on
#' phrase("b2 c d", "4( 4)- 2", "5 5 5") # hammer and slide
#'
#' phrase("c ec'g' ec'g'", "1 1 1", "5 432 432")
#' p("c ec'g' ec'g'", "1 1 1", "5 432 432") # same as above
#'
#' n <- "a, b, c d e f g e f g a~ a"
#' i <- "8- 8 8 8] t8( t8)( t8) t16( t16)( t16) 8 1"
#' m <- as_music(n, i)
#'
#' x <- p(n, i)
#' x
#' identical(x, p(m))
phrase <- function(notes, info = NULL, string = NULL, bar = FALSE){
  if(is.null(info)){
    if(!inherits(notes, "music")) notes <- as_music(notes)
    info <- .uncollapse(music_info(notes))
    notes <- .uncollapse(music_notes(notes))
  } else {
    .check_noteworthy(notes)
  }
  notes <- .uncollapse(notes)
  n <- length(notes)
  idx <- grep("\\d", notes)
  if(length(idx)) notes <- .octave_to_tick(notes)
  info <- .uncollapse(info)
  if(length(info) == 1) info <- rep(info, n)
  if(length(notes) != length(info))
    stop(paste("`info` must have the same number of timesteps as `notes`",
               "or a single value to repeat."), call. = FALSE)

  if(length(string) == 1 && is.na(string)) string <- NULL
  if(!is.null(string)){
    string <- .uncollapse(string)
    if(length(string) == 1) string <- rep(string, n)
    if(length(string) != length(notes))
      stop(paste("`string` must have the same number of timesteps as `notes`,",
                 "or a single value to repeat, or be NULL."), call. = FALSE)
  }

  dur <- as.character(info_duration(info))
  trp <- gsub("t", "", gsub("^\\d+(\\.+|)$", "", dur))
  rl <- rle(trp)

  x <- purrr::map(seq_along(rl$values), ~{
    idx2 <- sum(rl$lengths[1:.x])
    idx1 <- idx2 - rl$lengths[.x] + 1
    idx <- idx1:idx2
    x <- notes[idx]
    y <- info[idx]
    z <- string[idx]
    v <- as.integer(rl$values[.x])
    p0 <- .phrase(x, y, z)
    if(!is.na(v)){
      p0 <- paste(p0, collapse = " ")
      if(bar) p0 <- paste(p0, "|")
      p0 <- gsub("\\| \\|", "\\|", p0)
      p0 <- gsub(">t", ">", triplet(as_phrase(p0), v))
    }
    p0
  })

  idx <- which(rl$values == "")
  if(length(idx)){
    x[idx] <- purrr::map(x[idx], ~{
      x <- paste(.x, collapse = " ")
      if(bar) x <- paste(x, "|")
      x <- gsub("\\| \\|", "\\|", x)
      as_phrase(x)
    })
  }
  do.call(c, x)
}

#' @export
#' @rdname phrase
p <- phrase

.phrase <- function(notes, info, string){
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
  gsub("\\\\x", "", x)
}

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
  x <- gsub("(\\\\deadNote|\\\\staccato|\\\\glissando)", info("\\1"), x)
  x <- gsub(">(\\d+)([\\.\\(\\)]+)( <|\\^|)",
            paste0(">", info("\\1\\2"), "\\3"), x)
  x <- gsub(">(\\d+|)(\\.+|)(\\\\[a-zA-Z]+|)",
            paste0(">", info("\\1\\2\\3")), x)
  x <- gsub("(\\^\\\\bendAfter #\\+6)", info("\\1"), x)
  x <- gsub("(~)", info("\\1"), x)
  x <- gsub("(r|s)(\\d+)", paste0(notes("\\1"), info("\\2")), x)
  x <- gsub("(\\\\tuplet \\d/\\d \\d+ \\{|\\})", info("\\1"), x)
  x <- gsub("(\\\\repeat )(unfold|percent|volta)( \\d+ \\{)",
            info("\\1\\2\\3"), x)
  cat(col1("<"), col1$bold("Musical phrase"), col1(">"), "\n", col1(x), "\n",
      sep = "")
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
#' @param annotations logical, strip any text annotations from the note info
#' converted from \code{phrase}.
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
  x <- gsub("\\^\".*\"", "", phrase)
  i1 <- sum(attr(gregexpr("<", x)[[1]], "match.length"))
  if(i1 < 1){
    if(gsub(" |(r|s)\\d+(\\.|)(\\.|)", "", x) == ""){
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
  i2 <- sum(attr(gregexpr(">", x)[[1]], "match.length"))
  if(i1 != i2) return(FALSE)
  TRUE
}

#' @export
#' @rdname phrase-checks
notify <- function(phrase){
  if(!phrasey(phrase)) stop("`phrase` is not phrasey.", call. = FALSE)
  x <- .tag_rests(phrase)
  x <- gsub("\\\\deadNote ", "<\\\\deadNote ", x)
  x <- strsplit(x, " <")[[1]]
  x <- gsub("\\\\glissando", "-", x)
  x <- gsub("\\\\staccato", "]", x)
  x <- gsub("is", "#", x)
  x <- gsub("(^|<)([a|e])s", "\\2_", x)
  x <- gsub(" ([a|e])s", " \\1_", x)
  x <- gsub("es", "_", x)

  txt <- rep("", length(x))
  idx <- grepl(".*\\^\".*\".*", x)
  if(any(idx)){
    txt[idx] <- gsub(" ", "_", gsub(".*\\^\"(.*)\".*", "\\1", x[idx]))
    x[idx] <- gsub("(.*)(\\^\".*)", "\\1", x[idx])
  }

  idx2 <- grep("^(<|)\\\\deadNote$", x)
  if(length(idx2)) x[idx2 + 1] <- paste0(x[idx2 + 1], "x")

  x <- gsub(" ", "", x)
  x <- gsub("^<", "", x)

  if(any(idx)) x[idx] <- paste0(x[idx], ";^\"", txt[idx], "\"")

  if(length(idx2)) x <- x[-idx2]

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
phrase_info <- function(phrase, collapse = TRUE, annotations = TRUE){
  x <- notify(phrase)$info
  if(!annotations) x <- .strip_annotations(x)
  if(collapse) x <- paste(x, collapse = " ")
  .asni(x)
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

.strip_annotations <- function(x){
  idx <- grepl(";\\^\".*\".*", x)
  if(any(idx)) x[idx] <- gsub("(.*);\\^\".*\"$", "\\1", x[idx])
  x
}
