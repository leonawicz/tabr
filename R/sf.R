#' Create a musical phrase from string/fret combinations
#'
#' Create a musical phrase from character strings that define string numbers, fret numbers and note metadata. This function is a wrapper around \code{\link{phrase}}.
#' It allows for specifying string/fret combinations instead of unambiguous pitch as is used by \code{phrase}.
#' In order to remove ambiguity, it is critical to specify the instrument string tuning and key signature.
#' See the main function \code{phrase} for more details.
#'
#' The function \code{sfp} is a convenient shorthand wrapper for \code{sf_phrase}.
#'
#' @param string character, string numbers associated with notes.
#' @param fret character, fret numbers associated with notes.
#' @param info character, metadata associated with notes.
#' @param tuning character, instrument tuning.
#' @param key character, key signature. Passed to \code{transpose} function.
#' @param style character, octave numbering style, tick or integer.
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
sf_phrase <- function(string, fret, info, tuning = "standard", key = "c", style = c("tick", "integer"), bar = FALSE){
  style <- match.arg(style)
  .check_phrase_input(string, "string")
  .check_phrase_input(fret, "fret")
  string <- paste(gsub("_", "", .strsub(string)), collapse = " ")
  fret <- (strsplit(fret, " ")[[1]] %>% purrr::map_chr(.star_expand) %>%
              paste0(collapse = " ") %>% strsplit(" "))[[1]]
  tuning <- .map_tuning(tuning)
  open_notes <- rev(strsplit(tuning, " ")[[1]])
  str_num <- rev(seq_along(open_notes))
  notes <- purrr::map2(strsplit(string, " ")[[1]], fret, ~({
    x <- as.integer(strsplit(.x, "")[[1]])
    y <- gsub(" $", "", gsub("(\\(\\d{2}\\))", "\\1 ", .y))
    y <- strsplit(y, " ")[[1]]
    y <- lapply(y, function(x){
      if(substr(x, 1, 1) == "(") gsub("\\(|\\)", "", x) else strsplit(x, "")[[1]]
      }) %>% unlist %>% as.integer
    if(length(x) != length(y)) stop("String/fret mismatch.")
    x <- sapply(seq_along(x), function(i, x, y) transpose(open_notes[x[i]], y[i], key, "integer"), x = x, y = y)
    paste(x, collapse = "")
  })) %>% unlist %>% paste(collapse = " ")
  notes <- if(style == "tick") .octavesub(notes) else gsub("3", "", notes)
  phrase(notes, info, string, bar)
}

#' @export
#' @rdname sf_phrase
sfp <- function(...) sf_phrase(...)
