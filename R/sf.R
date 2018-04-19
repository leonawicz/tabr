# nolint start

#' Create a musical phrase from string/fret combinations
#'
#' Create a musical phrase from character strings that define string numbers, fret numbers and note metadata. This function is a wrapper around \code{\link{phrase}}.
#' It allows for specifying string/fret combinations instead of unambiguous pitch as is used by \code{phrase}.
#' In order to remove ambiguity, it is critical to specify the instrument string tuning and key signature.
#' It essentially uses \code{string} and \code{fret} in combination with a known tuning and key signature in order to generate \code{notes} for \code{\link{phrase}}.
#' \code{info} is passed straight through to \code{phrase}, as is \code{string} once it is done being used to help inform \code{notes}.
#'
#' See the main function \code{phrase} for more details. If you landed here first and are not familiar with \code{phrase}, be aware that \code{sf_phrase} is a tangential extra feature wrapper function in \code{tabr} and for a variety of reasons (see below) the approach it uses is discouraged in general.
#' If this is your only option, take note of the details and limitations below.
#'
#' This function is a crutch for users not working with musical notes (what to play), but rather just position on the guitar neck (where to play). This method has its conveniences, but it is inherently limiting.
#' In order to remove ambiguity, it is necessary to specify the instrument tuning and the key signature (or at least whether non-natural notes in the output should be sharps or flats).
#'
#' In the standard approach where you specify what to play, specifying exactly where to play is optional, but highly recommended (by providing \code{string}). Here \code{string} is of course required along with \code{fret}.
#' But any time the tuning changes, this "where to play" method breaks down and must be redone. It is much more robust to provide the string and pitch rather than the string and fret.
#' The key is always important because it is the only way to indicate if non-natural notes are sharps or flats.
#'
#' This crutch method also increases redundancy and typing. In order to specify rests \code{r}, silent rests \code{s}, and tied notes \code{~}, these must now be providing in parallel in both the \code{string} and \code{fret} arguments,
#' whereas in the standard method using \code{phrase}, they need only be provided once to \code{notes}.
#' A mismatch will throw an error. Despite the redundancy, this is helpful for ensuring proper match up between \code{string} and \code{fret}, which is essentially a dual entry method that aims to reduce itself inside \code{sf_phrase} to a single \code{notes} string that is passed internally to \code{phrase}.
#'
#' The important thing to keep in mind is that by its nature, this method of writing out music does not lend itself well to high detail.
#' Tabs that are informed by nothing but string and fret number remove a lot of important information, and those that attempt to compensate with additional symbols in say, an ascii tab, are difficult to read.
#' This wrapper function providing this alternative input method does its job of allowing users to create phrase objects that are equivalent to standard \code{phrase}-generated objects, including rests and ties, but practice and comfort with working with \code{phrase} and not this wrapper is highly recommended,
#' not just for eventual ease of use but for not preventing yourself from learning your way around the guitar neck and where all the different pitches are located.
#'
#' The function \code{sfp} is a convenient shorthand wrapper for \code{sf_phrase}. \code{sf_note} and the alias \code{sfn} are wrappers around \code{sf_phrase} that force \code{to_notes = TRUE}.
#'
#' @param string character, string numbers associated with notes.
#' @param fret character, fret numbers associated with notes.
#' @param info character, metadata associated with notes.
#' @param key character, key signature or just specify \code{"sharp"} or \code{"flat"}.
#' @param tuning character, instrument tuning.
#' @param to_notes logical, return only the mapped notes character string rather than the entire phrase object.
#' @param bar logical, insert a bar check at the end of the phrase.
#' @param ... arguments passed to \code{sf_phrase}.
#'
#' @return a phrase.
#' @seealso \code{\link{phrase}}
#' @export
#'
#' @examples
#' sf_phrase("5 4 3 2 1", "1 3 3 3 1", "8*4 1", key = "b_")
#' sf_phrase("654321 6s 12 1 21", "133211 355333 11 (13) (13)(13)", "4 4 8 8 4", key = "f")
#' sfp("6s*2 1*4", "000232*2 2*4", "4 4 8*4", tuning = "dropD", key = "d")
sf_phrase <- function(string, fret, info, key = "c", tuning = "standard", to_notes = FALSE, bar = FALSE){
  .check_phrase_input(string, "string")
  .check_phrase_input(fret, "fret")
  string <- paste(gsub("_", "", .strsub(string)), collapse = " ")
  fret <- (strsplit(fret, " ")[[1]] %>% purrr::map_chr(.star_expand) %>%
              paste0(collapse = " ") %>% strsplit(" "))[[1]]
  tuning <- .map_tuning(tuning)
  open_notes <- rev(strsplit(tuning, " ")[[1]])
  str_num <- rev(seq_along(open_notes))
  notes <- purrr::map2(strsplit(string, " ")[[1]], fret, ~({
    string_tie <- grepl("~", .x)
    fret_tie <- grepl("~", .y)
    if(!identical(string_tie, fret_tie)) stop("Tied note mismatch.")
    x <- if(any(string_tie)) gsub("~", "", .x) else .x
    y <- if(any(fret_tie)) gsub("~", "", .y) else .y
    rests <- c("r", "s")
    if(x %in% rests | y %in% rests){
      if(x == y) return(x) else stop("Rest mismatch.")
    }
    x <- as.integer(strsplit(x, "")[[1]])
    if(any(!x %in% str_num)) stop("String number outside range inferred by tuning.")
    y <- gsub(" $", "", gsub("(\\(\\d{2}\\))", "\\1 ", y))
    y <- strsplit(y, " ")[[1]]
    y <- lapply(y, function(x){
      if(substr(x, 1, 1) == "(") gsub("\\(|\\)", "", x) else strsplit(x, "")[[1]]
      }) %>% unlist %>% as.integer
    if(length(x) != length(y)) stop("String/fret mismatch.")
    x <- sapply(seq_along(x), function(i, x, y) transpose(open_notes[x[i]], y[i], key, "tick"), x = x, y = y)
    if(any(string_tie)) x[string_tie] <- paste0(x[string_tie], "~")
    paste(x, collapse = "")
  })) %>% unlist %>% paste(collapse = " ")
  if(to_notes) return(notes)
  phrase(notes, info, gsub("~", "", string), bar)
}

# nolint end

#' @export
#' @rdname sf_phrase
sfp <- function(...) sf_phrase(...)

#' @export
#' @rdname sf_phrase
sf_note <- function(...){
  o <- list(...)
  o$to_notes <- TRUE
  do.call(sf_phrase, o)
}

#' @export
#' @rdname sf_phrase
sfn <- function(...) sf_note(...)
