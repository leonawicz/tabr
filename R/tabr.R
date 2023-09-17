globalVariables(c(".data", ":="))

#' tabr: Music notation syntax, manipulation, analysis and transcription in R.
#'
#' The `tabr` package provides a music notation syntax and a collection of music
#' programming functions for generating, manipulating, organizing and analyzing
#' musical information in R. The music notation framework facilitates creating
#' and analyzing music data in notation form.
#'
#' Music syntax can be entered directly in character strings, for example to
#' quickly transcribe short pieces of music. The package contains functions for
#' directly performing various mathematical, logical and organizational
#' operations and musical transformations on special object classes that
#' facilitate working with music data and notation. The same music data can be
#' organized in tidy data frames for a familiar and powerful approach to the
#' analysis of large amounts of structured music data. Functions are available
#' for mapping seamlessly between these formats and their representations of
#' musical information.
#'
#' The package also provides an API to 'LilyPond' (<https://lilypond.org/>) for
#' transcribing musical representations in R into tablature ("tabs") and sheet
#' music. 'LilyPond' is open source music engraving software for generating high
#' quality sheet music based on markup syntax. The package generates 'LilyPond'
#' files from R code and can pass them to the 'LilyPond' command line interface
#' to be rendered into sheet music PDF files or inserted into R markdown
#' documents.
#'
#' The package offers nominal MIDI file output support in conjunction with
#' rendering sheet music. The package can read MIDI files and attempts to
#' structure the MIDI data to integrate as best as possible with the data
#' structures and functionality found throughout the package.
#'
#' `tabr` offers a useful but limited LilyPond API and is not intended to
#' access all LilyPond functionality from R,
#' nor is transcription via the API the entire scope of `tabr`.
#' If you are only creating sheet music on a case by case basis, write your own
#' LilyPond files manually.
#' There is no need to use `tabr` or limit yourself to its existing
#' LilyPond API.
#' If you are generating music notation programmatically,
#' `tabr` provides the ability to do so in R and has the added benefit of
#' converting what you write in R code to the LilyPond file format to be
#' rendered as printable guitar tablature.
#'
#' While LilyPond is listed as a system requirement for `tabr`, you can
#' use the package for music analysis without installing LilyPond if you do not
#' intend to render tabs.
#'
#' @docType package
#' @name tabr
#' @aliases tabr-package
NULL

#' @importFrom tibble tibble
NULL

.uncollapse <- function(x){
  x <- as.character(x)
  if(length(x) == 1) x <- strsplit(x, " ")[[1]]
  idx <- grep("\\*\\d+", x)
  if(length(idx)){
    f <- function(x){
      x <- strsplit(x, "\\*")[[1]]
      rep(x[1], as.integer(x[2]))
    }
    x <- as.list(x)
    x[idx] <- lapply(x[idx], f)
    unlist(x)
  } else {
    x
  }
}

.split_chords <- function(x){
  if(length(x) > 1) x <- paste(x, collapse = " ")
  strsplit(x, "(?<=.)(?=[a-grs ])", perl = TRUE)[[1]]
}

.infer_types <- function(x){
  list(o = .infer_octave_type(x), a = .infer_accidentals(x))
}

.infer_octave_type <- function(x){
  if(note_has_integer(x) & !note_has_tick(x)) "integer" else "tick"
}

.infer_accidentals <- function(x){
  if(any(note_has_sharp(x)) & !any(note_has_flat(x))) "sharp" else "flat"
}

.infer_time_format <- function(x){
  if(length(as.character(x)) == 1) "space" else "vector"
}

.guess_string_type <- function(x, try_info = TRUE){
  y <- tryCatch(
    as_noteworthy(x),
    error = function(e) NULL
  )
  if(!is.null(y)) return("noteworthy")
  if(try_info){
    y <- tryCatch(
      as_noteinfo(x),
      error = function(e) NULL
    )
    if(!is.null(y)) return("noteinfo")
  }
  y <- tryCatch(
    .check_music_split(x),
    error = function(e) NULL
  )
  if(!is.null(y)){
    "music"
  } else if(try_info){
    stop(paste("Cannot coerce string to any of class",
               "'noteworthy', 'noteinfo', or 'music'."), call. = FALSE)
  } else {
    stop("Cannot coerce string to 'noteworthy' or 'music'.", call. = FALSE)
  }
}

.lp_version <- function(){
  lp <- tabr_options()$lilypond
  if(lp == ""){
    if(Sys.info()[["sysname"]] == "Windows"){
      lp <- "lilypond.exe"
    } else {
      lp <- "lilypond"
    }
  }
  x <- tryCatch(
    system(paste(lp, "--version"), intern = TRUE), error = function(e) ""
  )
  if(!(length(x) == 1 && x != ""))
    x <- paste0(
      "\\version \"",
      gsub("^GNU LilyPond ([\\d+.]+).*", "\\1", x[1], perl = TRUE),
      "\"\n"
    )
  x
}

.check_lilypond <- function(){
  v <- .lp_version()
  if(length(v) > 1) stop("Cannot parse local LilyPond version.", call. = FALSE)
  if(v == "") stop(.lp_not_found, call. = FALSE)
  if(grepl("^[\\d.]+$", v, perl = TRUE))
    stop("Cannot parse local LilyPond version.", call. = FALSE)
  invisible()
}

.lp_not_found <- "Local LilyPond installation not found."
