#' Note info helpers
#'
#' Functions for working with note info strings.
#'
#' @param phrase a phrase object.
#'
#' @return character
#' @export
#' @name noteinfo
#'
#' @examples
#' a <- notate("8x", "Start here")
#' notes <- "a, b, c d e f g# a r ac'e' a c' e' c' r*3 ac'e'~ ac'e'"
#' info <- paste(a, "8] 8] 16 4.. 16- 16 2 2 4. 8( 4)( 4) 8*4 1 1")
#' p1 <- phrase(notes, info)
#'
#' phrase_duration(p1)
phrase_duration <- function(phrase){
  x <- phrase_info(phrase, FALSE, FALSE)
  gsub("^([0-9\\.]+).*", "\\1", x)
}

