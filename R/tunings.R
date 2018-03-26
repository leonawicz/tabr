#' Predefined instrument tunings.
#'
#' A data frame containing some predefined instrument tunings commonly used for guitar, bass, mandolin, banjo, ukulele and orchestral instruments.
#'
#' @format A data frame with 2 columns for the tuning ID and corresponding pitches and 32 rows for all predefined tunings.
"tunings"

.map_tuning <- function(x){
  if(x %in% tabr::tunings$id) return(tabr::tunings$value[x == tabr::tunings$id])
  if(!length(grep(" ", x)) || length(grep("[h-zA-Z]", x))) stop("Invalid `tuning`.")
  x
}
