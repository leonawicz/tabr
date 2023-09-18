#' LilyPond installation information
#'
#' Details about local LilyPond installation and package API.
#'
#' Version information and installation directory are returned if the
#' installation can be found. The LilyPond API references the currently loaded
#' version of `tabr`.
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
  os <- Sys.info()[["sysname"]]
  if(grepl("Darwin", os, ignore.case = TRUE)) os <- "MacOS"
  x <- switch(
    os,
    Windows = .tabr_api_lp_versions$Windows,
    Linux = .tabr_api_lp_versions$Linux,
    MacOS = .tabr_api_lp_versions$MacOS
  )
  x <- paste("LilyPond", x)

  msg <- paste0(
    "The tabr ", getNamespaceVersion("tabr"),
    " LilyPond API was built and tested against ", x, " on ", os, "."
  )
  message(msg)
}

.tabr_api_lp_versions <- list(
  Windows = "2.23.6",
  Linux   = "2.22.1-2",
  MacOS   = "2.24.2"
)
