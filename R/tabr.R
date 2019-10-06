globalVariables(".data")

#' tabr: Music notation syntax, manipulation, analysis and transcription in R.
#'
#' The \code{tabr} package provides a music notation syntax and a collection of
#' music programming functions for generating, manipulating, organizing and
#' analyzing musical information in R.
#' The package also provides API wrapper functions for transcribing musical
#' representations in R into guitar tablature ("tabs") using the 'LilyPond'
#' backend <http://lilypond.org>.
#'
#' LilyPond is an open source music engraving program for generating high
#' quality sheet music based on markup syntax.
#' \code{tabr} generates LilyPond files from R code and can pass them to
#' LilyPond to be rendered into sheet music pdf files from R.
#'
#' While LilyPond caters to sheet music in general and \code{tabr} can be used
#' to create basic sheet music,
#' the transcription functions focus on leveraging LilyPond specifically for
#' creating quality guitar tablature.
#'
#' \code{tabr} offers a useful but limited LilyPond API and is not intended to
#' access all LilyPond functionality from R,
#' nor is transcription via the API the entire scope of \code{tabr}.
#' If you are only creating sheet music on a case by case basis, write your own
#' LilyPond files manually.
#' There is no need to use \code{tabr} or limit yourself to its existing
#' LilyPond API.
#' If you are generating music notation programmatically,
#' \code{tabr} provides the ability to do so in R and has the added benefit of
#' converting what you write in R code to the LilyPond file format to be
#' rendered as printable guitar tablature.
#'
#' While LilyPond is listed as a system requirement for \code{tabr}, you can
#' use many of its functions without installing LilyPond if you do not intend
#' to render tabs.
#'
#' @docType package
#' @name tabr
NULL

#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

#' @importFrom dplyr tibble
NULL

.uncollapse <- function(x){
  if(length(x) == 1) x <- strsplit(as.character(x), " ")[[1]]
  f <- function(x){
    if(!grepl("\\*\\d+", x)) return(x)
    x <- strsplit(x, "\\*")[[1]]
    rep(x[1], as.integer(x[2]))
  }
  unlist(lapply(x, f))
}
