#' Noteworthy string to data frame
#'
#' Convert a noteworthy string to a tibble data frame and include additional
#' derivative variables.
#'
#' If `info` is provided or `notes` is a phrase object, the resulting data frame
#' also contains note durations and other info variables. The `duration` column
#' is always included in the output even as a vector of `NA`s when `info = NULL`.
#' This makes it more explicit that a given music data frame was generated
#' without any time information for the timesteps. Other note info columns are
#' not included in this case.
#'
#' For some derived column variables the root note (lowest pitch) in chord is
#' used. This is done for pitch intervals and scale intervals between adjacent
#' timesteps. This also occurs for scale degrees.
#'
#' `chord = "root"` additionally collapses columns like semitone, octave, and
#' frequency to the value for the root note so that all rows contain one numeric
#' value. `chord = "list"` retains full information as list columns.
#' `chord = "character"` collapses into strings so that values are readily
#' visible when printing the table, but information is not stripped and can be
#' recovered without recomputing from the original pitches.
#'
#' @param notes character, a noteworthy string. Alternatively, a music object
#' or a phrase object, in which case `info` is ignored.
#' @param info `NULL` or character, a note info string.
#' @param key character, key signature, only required for inclusion of scale
#' degrees.
#' @param scale character, defaults to `"diatonic"`. Only used in conjunction
#' with `key`, this can be used to alter scale degrees. Not any arbitrary
#' combination of valid `key` and valid `scale` is valid. See [scale_degree()].
#' @param si_format character, format for scale intervals. See
#' [scale_interval()].
#' @param chords character, how to structure columns containing multiple values
#' per chord/row of data frame. See details.
#'
#' @return a tibble data frame
#' @export
#'
#' @examples
#' x <- "a, b, c d e f g# a r ac'e' a c' e' c' r r r a"
#' as_music_df(x, key = "c", scale = "major")
#' as_music_df(x, key = "am", scale = "harmonic_minor", si_format = "ad_abb")
#'
#' a <- notate("8", "Start here.")
#' time <- paste(a, "8^*2 16-_ 4.. 16( 16)( 2) 2 4. t8- t8 t8- 8[accent]*4 1")
#' d1 <- as_music_df(x, time)
#' d1
#'
#' # Go directly from music object to data frame
#' m1 <- as_music(x, time)
#' d2 <- as_music_df(m1)
#' identical(d1, d2)
#'
#' # Go directly from phrase object to data frame
#' p1 <- phrase("a b cgc'", "4-+ 4[accent] 2", 5)
#' identical(as_music_df(as_music("a4-+;5 b[accent] cgc'2")), as_music_df(p1))
as_music_df <- function(notes, info = NULL, key = NULL, scale = "diatonic",
                        chords = c("root", "list", "character"),
                        si_format = c("mmp_abb", "mmp", "ad_abb", "ad")){
  .keycheck(key)
  si_format <- match.arg(si_format)
  if(inherits(notes, "phrase")){
    x <- phrase_notes(notes, FALSE)
    info <- phrase_info(notes, FALSE)
    string <- phrase_strings(notes, FALSE)
    if(all(is.na(string))) string <- NULL
    len <- length(x)
  } else if(inherits(notes, "music")){
    x <- .uncollapse(music_notes(notes))
    info <- .uncollapse(music_info(notes))
    string <- music_strings(notes)
    len <- length(x)
  } else {
    .check_noteworthy(notes)
    x <- .uncollapse(notes)
    len <- length(x)
    string <- NULL
    if(!is.null(info)){
      .check_noteinfo(info)
      info <- .uncollapse(info)
      if(length(info) == 1) info <- rep(info, len)
      if(length(info) != len)
        stop(paste("`info` must have the same number of timesteps as `notes`,",
                   "or a single value to repeat, or be NULL."), call. = FALSE)
    }
  }
  if(length(grep("\\d", x))) x <- .octave_to_tick(x)
  n <- .pitch_to_note(x)
  o <- octaves(x)
  s <- unname(chord_semitones(n))
  f <- chord_freq(x)
  if(is.null(info)){
    duration <- NA_character_
  } else {
    duration <- as.character(info_duration(info))
    articulation <- info_articulation(info)
    slur <- rep(NA_character_, len)
    y <- info_slur_on(info)
    if(any(y)) slur[y] <- "on"
    y <- info_slur_off(info)
    if(any(y)) slur[y] <- ifelse(is.na(slur[y]), "off", "hold")
    slide <- info_slide(info)
    bend <- info_bend(info)
    dotted <- rep(0L, len)
    y <- info_dotted(info)
    if(any(y)) dotted[y] <- 1L
    y <- info_double_dotted(info)
    if(any(y)) dotted[y] <- 2L
    ann <- info_annotation(info)
  }

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

  if(is_noteworthy(x)) x <- as.character(x)
  if(is_noteworthy(n)) n <- as.character(n)
  d <- tibble::tibble(duration = duration, pitch = x, note = n, semitone = s,
                      octave = o, freq = f)
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
  if(!is.null(info)){
    d <- dplyr::mutate(
      d, slur = slur, slide = slide, bend = bend, dotted = dotted,
      articulation = articulation, annotation = ann)
  }
  if(!is.null(string)) d <- dplyr::mutate(d, string = string)
  d
}
