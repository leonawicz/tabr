#' Options
#'
#' Options for tabr package.
#'
#' Currently only \code{lilypond}, \code{midi2ly} and \code{python} are used. On Windows systems, if the system path for lilypond.exe, midi2ly.py and python.exe are not stored in the system PATH environmental variable, they must be provided by the user after loading the package.
#'
#' @param ... a list of options.
#'
#' @return The function prints all set options if called with no arguments. When setting options, nothing is returned.
#' @export
#'
#' @examples
#' tabr_options()
#' tabr_options(lilypond = "C:/Program Files (x86)/LilyPond/usr/bin/lilypond.exe")
tabr_options <- function(...){
  x <- list(...)
  opts <- .tabr_env$opts
  if(length(x)){
      if(length(x) > 0){
        opts[names(x)] <- x
        .tabr_env$opts <- opts
      }
      invisible()
  } else {
    opts
  }
}

.tabr_env <- new.env()
