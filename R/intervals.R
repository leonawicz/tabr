#' Interval semitones
#'
#' Convert named intervals to numbers of semitones.
#' For a complete list of valid interval names and abbreviations see \code{\link{mainIntervals}}.
#' \code{interval} may be a vector.
#'
#' @param interval character, interval ID. See details.
#'
#' @return integer
#' @export
#' @seealso \code{\link{mainIntervals}}
#'
#' @examples
#' x <- c("minor third", "m3", "augmented second", "A2")
#' y <- c("P1", "m2", "M2", "m3", "M3", "P4", "TT", "P5")
#' interval_semitones(x)
#' interval_semitones(y)
interval_semitones <- function(interval){
  d <- tabr::mainIntervals
  idx <- sapply(interval, function(x){
    i <- which(d[, -1] == x, arr.ind = TRUE)
    if(length(i)) i[1] else as.integer(NA)
  })
  d$semitones[idx]
}

#' Interval helpers
#'
#' Helper functions for musical intervals defined by two notes.
#'
#' Intervals are directional. \code{pitch_interval} returns the number of semitones defining the distance between two notes. The interval is negative if \code{note1} has higher pitch than \code{note2}.
#' For \code{scale_interval}, a character string is returned that provides the named main interval, simple or compound, defined by  the two notes.
#' This function will return \code{NA} for any uncommon interval not listed in \code{\link{mainIntervals}}.
#'
#' @param note1 character, first note. Must be a single note.
#' @param note2 character, second note.
#' @param format character, format of the scale notation: major/minor/perfect, augmented/diminished, and respective abbreviations. See argument options in defaults.
#' @param ignore_octave logical, reduce the interval to that defined by notes within a single octave.
#' @param tuning character, string tuning.
#'
#' @return a musical interval, integer or character depending on the function.
#' @export
#' @seealso \code{\link{mainIntervals}}
#' @name interval-helpers
#'
#' @examples
#' pitch_interval("b", "c4")
#' pitch_interval("c,", "d")
#' scale_interval("c", "e_")
pitch_interval <- function(note1, note2, ignore_octave = FALSE){
  .check_note(note1)
  .check_note(note2)
  ogap <- .octave_interval(note1, note2, ignore_octave)
  note1 <- .pitch_to_note(note1)
  note2 <- .pitch_to_note(note2)
  n1_nat <- .pitch_natural(note1)
  n2_nat <- .pitch_natural(note2)
  n1_sharp <- .pitch_sharp(note1)
  n2_sharp <- .pitch_sharp(note2)
  if(!n1_nat && !n2_nat && n2_sharp != n1_sharp)
    note2 <- ifelse(n2_sharp, flatten_sharp(note2), sharpen_flat(note2))
  sharp <- grepl("#", paste(note1, note2))
  x <- scale_chromatic(note1, sharp = sharp, ignore_octave = TRUE)
  ngap <- match(note2, x) - match(note1, x)
  12 * (ogap - .cpass(note1, note2, x)) + ngap
}

#' @export
#' @rdname interval-helpers
scale_interval <- function(note1, note2, format = c("mmp_abb", "mmp", "ad_abb", "ad"), ignore_octave = FALSE){
  format <- match.arg(format)
  semitones <- abs(pitch_interval(note1, note2, ignore_octave))
  d <- tabr::mainIntervals
  x <- d[d$semitones == semitones, format]
  if(!length(x)) x <- as.character(NA)
  x
}

#' @export
#' @rdname interval-helpers
tuning_intervals <- function(tuning = "standard"){
  x <- strsplit(.map_tuning(tuning), " ")[[1]]
  c(0, cumsum(sapply(1:(length(x) - 1), function(i) pitch_interval(x[i], x[i + 1]))))
}

.octave_interval <- function(x1, x2, ignore_octave = FALSE){
  if(ignore_octave){
    x1 <- gsub("\\d+|,|'", "", x1)
    x2 <- gsub("\\d+|,|'", "", x2)
  }
  .pitch_to_octave(x2) - .pitch_to_octave(x1)
}

.pitch_to_octave <- function(x){
  if(grepl("\\d+", x)){
    octave <- as.integer(gsub("[a-g#_~]+(\\d+).*", "\\1", x))
  } else if(grepl(",", x)){
    octave <- -length(gregexpr(",", x)[[1]]) + 3
  } else if(grepl("'", x)){
    octave <- length(gregexpr("'", x)[[1]]) + 3
  } else {
    octave <- 3L
  }
  octave
}

.pitch_to_note <- function(x) gsub("\\d|,|'", "", x)
.pitch_accidental <- function(x) grepl("_|#", x)
.pitch_natural <- function(x) !.pitch_accidental(x)
.pitch_sharp <- function(x) grepl("#", x)
.pitch_flat <- function(x) grepl("_", x)

.cpass <- function(x1, x2, scale){
  idx1 <- match(x1, scale)
  idx2 <- match(x2, scale)
  if("c" %in% scale){
    idxc <- match("c", scale)
  } else if("c#" %in% scale){
    idxc <- match("c#", scale)
  } else {
    idxc <- match("d_", scale)
  }
  (idx1 < idxc & idx2 >= idxc) | (idx1 >= idxc & idx2 < idxc)
}
