#' Noteworthy string to data frame
#'
#' Convert a noteworthy string to a tibble data frame and include additional
#' derivative variables.
#'
#' For some derived column variables the root note (lowest pitch) in chord is
#' used. This is done for pitch intervals and scale intervals between adjacent
#' timesteps. This also occurs for scale degrees.
#'
#' \code{chord = "root"} additionally collapses columns like semitone, octave,
#' and frequency to the value for the root note so that all rows contain one
#' numeric value. \code{chord = "list"} retains full information as list
#' columns. \code{chord = "character"} collapses into strings so that values are
#' readily visible when printing the table, but information is not stripped and
#' can be recovered without recomputing from the original pitches.
#'
#' @param notes character, a noteworthy string.
#' @param key character, key signature, only required for inclusion of scale
#' degrees.
#' @param scale character, defaults to \code{"diatonic"}. Only used in
#' conjunction with \code{key}, this can be used to alter scale degrees. Not
#' any arbitrary combination of valid \code{key} and valid \code{scale} is
#' valid. See \code{\link{scale_degree}}.
#' @param si_format character, format for scale intervals. See
#' \code{\link{scale_interval}}.
#' @param chords character, how to structure columns containing multiple values
#' per chord/row of data frame. See details.
#'
#' @return a tibble data frame
#' @export
#'
#' @examples
#' x <- "a, b, c d e f g# a ac'e' a c' e' c' a"
#' as_music_df(x, key = "c", scale = "major")
#' as_music_df(x, key = "am", scale = "harmonic_minor", si_format = "ad_abb")
as_music_df <- function(notes, key = NULL, scale = "diatonic",
                        chords = c("root", "list", "character"),
                        si_format = c("mmp_abb", "mmp", "ad_abb", "ad")){
  .keycheck(key)
  si_format <- match.arg(si_format)
  .check_noteworthy(notes)
  x <- .uncollapse(notes)
  if(length(grep("\\d", x))) x <- .octave_to_tick(x)
  n <- .pitch_to_note(x)
  o <- octaves(x)
  s <- unname(chord_semitones(n))
  f <- chord_freq(x)

  chords <- match.arg(chords)
  if(chords == "root"){
    o <- sapply(o, "[", 1)
    s <- sapply(s, "[", 1)
    f <- sapply(f, "[", 1)
  } else if(chords == "character"){
    o <- sapply(o, paste, collapse = ":")
    s <- sapply(s, paste, collapse = ":")
    f <- purrr::map_chr(f, ~paste(round(.x, 4), collapse = ":"))
  }

  d <- tibble::tibble(pitch = x, note = n, semitone = s, octave = o, freq = f)
  if(!is.null(key)){
    d <- dplyr::mutate(
      d, key = key, scale = scale,
      scale_deg = scale_degree(n, key[1], scale[1],
                                  strict_accidentals = FALSE)
    )
  }
  d <- dplyr::mutate(
    d, pitch_int = pitch_diff(x, TRUE),
    scale_int = scale_diff(x, TRUE, format = si_format))
  d
}
