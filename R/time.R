#' Summarize rhythm and time of music objects
#'
#' These functions assist with summarizing temporal data for music objects.
#'
#' These functions also work with the simpler \code{noteinfo} class, though
#' some functions require you to provide additional arguments.
#'
#' Functions that deal with real time require a known tempo, which music
#' objects have. The simpler note info object does not contain this information.
#' You can provide a value to the \code{tempo} argument of such functions. This
#' overrides the tempo of \code{x} if a music object. But the reason to use
#' \code{tempo} is to provide one when \code{x} is a note info object.
#' By default \code{tempo = NULL}, in which case it will derive the value from
#' the music object or return an error for note info objects.
#'
#' \code{n_measures} gives the total number of measures covered by all
#' timesteps. Functions providing the number of beats and beats per minute both
#' take a \code{unit}, defaulting to 4 for quarter note beats. The unit can be
#' any even beat, triplet beat, dotted, or double dotted beat, from
#' \code{"t32"} up to 1.
#'
#' The number of timesteps starting in each measure is obtained with
#' \code{steps_per_measure}.
#'
#' @param x note info or music object.
#' @param unit character, or an equivalent integer. A beat unit. See details.
#' @param tempo character, LilyPond format tempo, e.g., "4 = 120" is 120
#' quarter note beats per minute.
#'
#' @return depends on function
#' @export
#'
#' @examples
#' a <- notate("t8x", "Start here")
#' notes <- "a, b, c d e f g# a r ac'e' a c' e' c' r*3 ac'e'~ ac'e'"
#' info <- paste(a, "t8x t8] 16 4.. 16- 16 2^ 2 4. 8( 4)( 4) 8*4 1 1")
#' info <- as_noteinfo(info)
#' x <- as_music(notes, info)
#'
#' n_measures(info) # fraction indicates incomplete final measure
#' n_measures(x)
#'
#' n_beats(x)
#' n_beats(x, 1)
#' n_beats(x, "t16")
#'
#' bpm(x)
#' bpm(x, "t8")
#'
#' seconds(x)
#' seconds(info, "4 = 120")
#' seconds(info, "2 = 60")
#' seconds(x, "4 = 100")
#'
#' steps_per_measure(x)
#' seconds_per_measure(x)
#' seconds_per_step(x)
#' steps_start_time(x)
n_measures <- function(x){
  n_beats(x, 1)
}

#' @export
#' @rdname n_measures
n_beats <- function(x, unit = 4){
  .check_time_inputs(x)
  sum(duration_to_ticks(info_duration(x))) / .tick_match(unit)
}

#' @export
#' @rdname n_measures
steps_per_measure <- function(x){
  .check_time_inputs(x)
  ticks <- duration_to_ticks(info_duration(x))
  x <- table(floor((cumsum(ticks) - ticks) / 1920) + 1)
  tibble::tibble(measure = as.integer(names(x)), steps = as.integer(x))
}

#' @export
#' @rdname n_measures
bpm <- function(x, unit = 4, tempo = NULL){
  tempo <- .check_time_inputs(x, tempo, TRUE)
  x <- as.integer(trimws(strsplit(tempo, "=")[[1]]))
  x <- x[2] / x[1]
  x * 1920 / .tick_match(unit)
}

#' @export
#' @rdname n_measures
seconds <- function(x, tempo = NULL){
  tempo <- .check_time_inputs(x, tempo, TRUE)
  60 * n_beats(x) / bpm(x, tempo = tempo)
}

#' @export
#' @rdname n_measures
seconds_per_measure <- function(x, tempo = NULL){
  tempo <- .check_time_inputs(x, tempo, TRUE)
  seconds(x, tempo) / n_measures(x)
}

#' @export
#' @rdname n_measures
seconds_per_step <- function(x, tempo = NULL){
  tempo <- .check_time_inputs(x, tempo, TRUE)
  duration_to_ticks(info_duration(x)) / (480 * bpm(x, tempo = tempo) / 60)
}

#' @export
#' @rdname n_measures
steps_start_time <- function(x, tempo = NULL){
  tempo <- .check_time_inputs(x, tempo, TRUE)
  x <- seconds_per_step(x, tempo = tempo)
  cumsum(x) - x
}

.tick_match <- function(unit){
  y <- .tick_table()
  i <- match(unit, names(y))
  if(is.na(i)) stop("Invalid `unit`.", call. = FALSE)
  as.numeric(y[i])
}

.check_time_inputs <- function(x, tempo = NULL, check_tempo = FALSE){
  if(!is_music(x) & !is_noteinfo(x))
    stop("`x` must be a noteinfo or music object.", call. = FALSE)
  if(check_tempo & !is_music(x) & is.null(tempo))
    stop("`x` must provide `tempo` to supplment a noteinfo object.",
         call. = FALSE)
  if(check_tempo & is.null(tempo)) tempo <- music_tempo(x)
  tempo
}
