#' Interval semitones
#'
#' Convert named intervals to numbers of semitones.
#' For a complete list of valid interval names and abbreviations see
#' \code{\link{mainIntervals}}. \code{interval} may be a vector.
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
    if(length(i)) i[1] else NA_integer_
  })
  d$semitones[idx]
}

#' Interval helpers
#'
#' Helper functions for musical intervals defined by two notes.
#'
#' Numeric intervals are directional. \code{pitch_interval} returns the signed
#' number of semitones defining the distance between two notes.
#' Named scale intervals are names only. Use pitch for direction.
#'
#' \code{scale_interval} returns a character string that provides the named
#' main interval, simple or compound, defined by  the two notes.
#' This function will return \code{NA} for any uncommon out of range large
#' interval not listed as a named interval in \code{\link{mainIntervals}}.
#'
#' \code{pitch_interval} and \code{scale_interval} compute intervals element-wise
#' between two noteworthy strings. \code{pitch_diff} and \code{scale_diff}
#' work similarly but compute lagged intervals on the elements in \code{notes}.
#'
#' @param notes,notes1,notes2 character, a noteworthy string. \code{notes1} and
#' \code{notes2} must have equal number of timesteps.
#' @param use_root logical, use lowest pitch in chord for pitch intervals or
#' scale intervals between adjacent timesteps. Otherwise intervals involving
#' chords are \code{NA}.
#' @param n integer, size of lag.
#' @param trim logical, trim the leading \code{NA} values from lagged intervals.
#' @param format character, format of the scale notation: major/minor/perfect,
#' augmented/diminished, and respective abbreviations. See argument options in
#' defaults.
#' @param tuning character, string tuning.
#'
#' @return a musical interval, integer or character depending on the function.
#' @export
#' @seealso \code{\link{mainIntervals}}
#' @name intervals
#'
#' @examples
#' pitch_interval("b", "c4")
#' pitch_interval("c,", "d")
#' pitch_interval("c,", "dfa")
#' scale_interval("c", "e_")
#' scale_interval("ceg", "egd'")
#'
#' x <- "a, b, c d e f g# ac'e' a c' e'"
#' pitch_diff(x)
#' pitch_diff(x, use_root = FALSE)
#' scale_diff(x)
#' scale_diff(x, n = 2, trim = TRUE, use_root = FALSE)
pitch_interval <- function(notes1, notes2, use_root = TRUE){
  x <- purrr::map(list(notes1, notes2), ~.uncollapse(gsub("~", "", .x)))
  len <- sapply(x, length)
  if(length(x[[1]]) != length(x[[2]]))
    stop("Inputs must have equal number of timesteps.", call. = FALSE)
  for(i in seq_along(x)){
    idx <- sapply(gregexpr("[a-g]", x[[i]]), length) > 1
    if(any(idx)){
      x[[i]][idx] <- if(use_root) .chord_root(x[[i]][idx]) else NA_character_
    }
  }
  .pitch_interval(x[[1]], x[[2]])
}

#' @export
#' @rdname intervals
pitch_diff <- function(notes, use_root = TRUE, n = 1, trim = FALSE){
  x <- dplyr::lag(.uncollapse(notes), n)
  x <- pitch_interval(x, notes, use_root)
  if(trim) x[-c(seq_len(max(1, n)))] else x
}

#' @export
#' @rdname intervals
scale_interval <- function(notes1, notes2, use_root = TRUE,
                           format = c("mmp_abb", "mmp", "ad_abb", "ad")){
  format <- match.arg(format)
  x <- abs(pitch_interval(notes1, notes2, use_root))
  x[is.na(x)] <- 99
  d <- tabr::mainIntervals
  x <- d[match(x, d$semitones), format]
  if(length(x)) x else NA_character_
}

#' @export
#' @rdname intervals
scale_diff <- function(notes, use_root = TRUE, n = 1, trim = FALSE,
                       format = c("mmp_abb", "mmp", "ad_abb", "ad")){
  format <- match.arg(format)
  x <- dplyr::lag(.uncollapse(notes), n)
  x <- scale_interval(x, notes, use_root, format)
  if(trim) x[-c(seq_len(max(1, n)))] else x
}

#' @export
#' @rdname intervals
tuning_intervals <- function(tuning = "standard"){
  x <- strsplit(.map_tuning(tuning), " ")[[1]]
  as.integer(c(0, cumsum(sapply(1:(length(x) - 1),
                                function(i) pitch_interval(x[i], x[i + 1])))))
}

.na_interval <- function(x, y) is.na(x) | is.na(y)
.rest_interval <- function(x, y) x %in% c("r", "s") | y %in% c("r", "s")

.octave_interval <- function(x1, x2, ignore_octave = FALSE){
  if(ignore_octave){
    x1 <- gsub("\\d+|,|'", "", x1)
    x2 <- gsub("\\d+|,|'", "", x2)
  }
  .pitch_to_octave(x2) - .pitch_to_octave(x1)
}

.cpass <- function(x1, x2, scale){
  idx1 <- match(x1, scale)
  idx2 <- match(x2, scale)
  idxc <- match("c", scale)
  (idx1 < idxc & idx2 >= idxc) | (idx1 >= idxc & idx2 < idxc)
}
