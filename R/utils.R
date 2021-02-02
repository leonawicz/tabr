#' LilyPond installation information
#'
#' Details about local LilyPond installation and package API.
#'
#' Version information and installation directory are returned if the
#' installation can be found. The LilyPond API references the currently loaded
#' version of \code{tabr}.
#'
#' @return a message or system standard output.
#' @export
#'
#' @examples
#' lilypond_root()
#' lilypond_version()
#' tabr_lilypond_api()
lilypond_root <- function(){
  x <- Sys.which("lilypond")
  if(x == "") return(message(.lp_not_found))
  message(normalizePath(x, winslash = "/"))
}

#' @rdname lilypond_root
#' @export
lilypond_version <- function(){
  x <- utils::capture.output(system("lilypond -v"))
  if(!length(x) || x != "[1] 0") return(message(.lp_not_found))
}

#' @rdname lilypond_root
#' @export
tabr_lilypond_api <- function(){
  x <- "GNU LilyPond 2.23.0"
  msg <- paste0(
    "The tabr ", getNamespaceVersion("tabr"),
    " LilyPond API was built and tested against ", x,
    " for Windows and Linux.\nLilypond is not officially supported for OSX."
  )
  message(msg)
}

.lp_not_found <- "Cannot find lilypond installation."
