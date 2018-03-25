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
#' @param silent if \code{TRUE}, no text will be printed above the staff to indicate the number of plays when greater than one repeat and \code{endings} is not \code{NULL}. This is useful when another staff already displays the text.
#'
#' @return a phrase.
#' @export
#'
#' @examples
#' x <- phrase("c ec'g' ec'g'", "4 4 2", "5 432 432")
#' e1 <- phrase("a", 1, 5)
#' e2 <- phrase("b", 1, 5)
#' volta(x) # one repeat or two plays
#' pct(x) # as above, simple percent repeat notation
#' volta(x, 1, list(e1, e2)) # one repeat with alternate ending
#' volta(x, 4, list(e1, e2)) # multiple repeats but with only one alternate ending
volta <- function(phrase, n = 1, endings = NULL, silent = FALSE){
  if(n > 1 & is.null(endings) & !silent){
    phrase <- strsplit(phrase, " ")[[1]]
    idx <- grep(">", phrase)
    if(length(idx)){
      idx <- min(idx)
      phrase[idx] <- paste0(phrase[idx], paste("^\"Play", n + 1, "times.\""))
    }
    phrase <- paste0(phrase, collapse = " ")
  }
  x <- paste("\\repeat volta", n + 1, "{", paste(phrase, collapse = " "), "| }\n")
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
