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
#' \code{pitch_interval} and \code{scale_interval} compute intervals
#' element-wise between two noteworthy strings. \code{pitch_diff} and
#' \code{scale_diff} work similarly but compute lagged intervals on the
#' elements in \code{notes}.
#'
#' @param notes,notes1,notes2 character, a noteworthy string. \code{notes1} and
#' \code{notes2} must have equal number of timesteps.
#' @param use_root logical, use lowest pitch in chord for pitch intervals or
#' scale intervals between adjacent timesteps. Otherwise intervals involving
#' chords are \code{NA}.
#' @param n integer, size of lag.
#' @param trim logical, trim the \code{n} leading \code{NA} values from lagged
#' intervals.
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
#' pitch_interval("c, e_, g_, a,", "e_, g_, a, c")
#' pitch_interval("c r", "dfa d")
#' pitch_interval("c r", "dfa d", use_root = FALSE)
#' scale_interval("c", "e_")
#' scale_interval("ceg", "egd'")
#'
#' x <- "a, b, c d e f g# ac'e' a c' e'"
#' pitch_diff(x)
#' pitch_diff(x, use_root = FALSE)
#' scale_diff(x)
#' scale_diff(x, n = 2, trim = TRUE, use_root = FALSE)
#'
#' # Lagged intervals respect rest timesteps.
#' # All timestep position including rests are retained.
#' # But the lag-n difference skips rest entries.
#' x <- "a, c r r r r g"
#' pitch_diff(x)
#' scale_diff(x)
#' pitch_diff(x, n = 2)
#' scale_diff(x, n = 2)
#' pitch_diff(x, n = 2, trim = TRUE)
#' scale_diff(x, n = 2, trim = TRUE)
pitch_interval <- function(notes1, notes2, use_root = TRUE){
  .check_noteworthy(notes1, na.rm = TRUE)
  .check_noteworthy(notes2, na.rm = TRUE)
  .get_pitch_interval(notes1, notes2, use_root)
}

.get_pitch_interval <- function(notes1, notes2, use_root = TRUE){
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
  .check_noteworthy(notes, na.rm = TRUE)
  x <- .uncollapse(notes)
  idx <- x %in% c("r", "s")
  if(any(idx)) x <- x[!idx]
  i <- .get_pitch_interval(dplyr::lag(x, n), x, use_root)
  if(any(idx)){
    x <- rep(NA_integer_, length(idx))
    x[!idx] <- i
  } else {
    x <- i
  }
  if(trim) x[-c(seq_len(max(1, n)))] else x
}

#' @export
#' @rdname intervals
scale_interval <- function(notes1, notes2, use_root = TRUE,
                           format = c("mmp_abb", "mmp", "ad_abb", "ad")){
  format <- match.arg(format)
  .check_noteworthy(notes1, na.rm = TRUE)
  .check_noteworthy(notes2, na.rm = TRUE)
  .get_scale_interval(notes1, notes2, use_root, format)
}

.get_scale_interval <- function(notes1, notes2, use_root = TRUE,
                           format = c("mmp_abb", "mmp", "ad_abb", "ad")){
  format <- match.arg(format)
  x <- abs(.get_pitch_interval(notes1, notes2, use_root))
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
  .check_noteworthy(notes, na.rm = TRUE)
  x <- .uncollapse(notes)
  idx <- x %in% c("r", "s")
  if(any(idx)) x <- x[!idx]
  i <- .get_scale_interval(dplyr::lag(x, n), x, use_root, format)
  if(any(idx)){
    x <- rep(NA_character_, length(idx))
    x[!idx] <- i
  } else {
    x <- i
  }
  if(trim) x[-c(seq_len(max(1, n)))] else x
}

#' @export
#' @rdname intervals
tuning_intervals <- function(tuning = "standard"){
  x <- .split_chords(.map_tuning(tuning))
  as.integer(c(0, cumsum(sapply(1:(length(x) - 1),
                                function(i) pitch_interval(x[i], x[i + 1])))))
}
