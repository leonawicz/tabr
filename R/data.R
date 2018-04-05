#' Predefined instrument tunings.
#'
#' A data frame containing some predefined instrument tunings commonly used for guitar, bass, mandolin, banjo, ukulele and orchestral instruments.
#'
#' @format A data frame with 2 columns for the tuning ID and corresponding pitches and 32 rows for all predefined tunings.
"tunings"

.map_tuning <- function(x){
  if(x %in% tabr::tunings$id) return(tabr::tunings$value[x == tabr::tunings$id])
  if(length(grep("[h-zA-Z]", x))) stop("Invalid `tuning`.")
  .octavesub(x)
}

#' tabr syntax.
#'
#' A data frame containing descriptions of syntax used in phrase construction in tabr.
#'
#' @format A data frame with 3 columns for syntax description, operators and examples.
"tabrSyntax"
