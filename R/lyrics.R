#' Create lyrics and check lyrics string validity
#'
#' Functions for creating and checking lyrics objects.
#'
#' The \code{lyrics} class is a simple class for arranging lyrics text by
#' timestep. Its structure and behavior aligns with that of the classes
#' \code{noteworthy}, \code{noteinfo} and \code{music}.
#'
#' \code{lyrical} is a trivial function that returns a scalar logical result
#' essentially for any object that inherits from character, though this check
#' may become more specific in the future.
#'
#' \code{as_lyrics} can be used to coerce to the \code{lyrics} class.
#' Coercion will fail if the string is not lyrical.
#' The \code{lyrics} class has its own \code{print} and \code{summary} methods.
#'
#' When \code{format = NULL}, the timestep delimiter format is inferred from
#' the lyrical string input.
#'
#' @param x character or lyrics object. For \code{lyrics_template}, an integer
#' or one of the classes \code{noteworthy}, \code{noteinfo} or \code{music} to
#' derive the number of timesteps from.
#' @param format \code{NULL} or character, the timestep delimiter format,
#' \code{"space"} or \code{"vector"}.
#'
#' @return depends on the function
#' @export
#' @name lyrics
#'
#' @examples
#' # space-delimited lyrics; use periods for timesteps with no lyric
#' x <- "These are the ly- rics . . . to this song"
#' is_lyrics(x)
#' lyrical(x)
#' as_lyrics(x)
#'
#' # character vector; empty, period or NA for no lyric
#' x <- c("These", "are", "the", "ly-", "rics",
#'        "", ".", NA, "to", "this", "song") #
#' as_lyrics(x)
#'
#' # generate empty lyrics object from noteworthy, noteinfo or music object
#' notes <- as_noteworthy("c d e d c r*3 e g c'")
#' x <- lyrics_template(notes)
#' x
#'
#' x[1:5] <- strsplit("These are the ly- rics", " ")[[1]]
#' x[9:11] <- c("to", "this", "song")
#' x
#'
#' summary(x)
#'
#' attributes(x)
lyrical <- function(x){
  if(is_lyrics(x)) return(TRUE)
  .check_lyrics(x, FALSE)
}

#' @export
#' @rdname lyrics
as_lyrics <- function(x, format = NULL){
  if(inherits(x, "lyrics") & is.null(format)) return(x)
  if(is.null(format)) format <- .infer_time_format(x)
  .check_lyrics(x)
  .check_format_arg(format)
  .aslyrics(x, format)
}

.aslyrics <- function(x, format = "space"){
  x <- .uncollapse(x)
  x[is.na(x) | x == ""] <- "."
  steps <- length(x)
  npause <- length(which(x == "."))
  nlyric <- steps - npause
  if(format == "space"){
    if(length(x) > 1) x <- paste(x, collapse = " ")
    format <- "space-delimited time"
  } else {
    format <- "vectorized time"
  }
  attributes(x) <- list(steps = steps, n_lyric = nlyric, n_pause = npause,
                        format = format)
  class(x) <- unique(c("lyrics", class(x)))
  x
}

#' @export
#' @rdname lyrics
is_lyrics <- function(x){
  inherits(x, "lyrics")
}

.check_lyrics <- function(x, err = TRUE){
  if(inherits(x, "character")){
    if(err) return(invisible()) else return(TRUE)
  }
  if(err)
    stop("Lyrics do not inherit from character.", call. = FALSE) else FALSE
}

#' @export
#' @rdname lyrics
lyrics_template <- function(x, format = NULL){
  if(is.numeric(x)) return(as_lyrics(rep(".", x), format))
  if(is_noteworthy(x) | is_noteinfo(x) | is_music(x)){
    if(is.null(format)) format <- if(is_space_time(x)) "space" else "vector"
    as_lyrics(rep(".", length(x)), format)
  } else {
    stop("`x` must be an integer, or `noteworthy`, `noteinfo` or `music`.",
         call. = FALSE)
  }
}

#' @export
print.lyrics <- function(x, ...){
  a <- attributes(x)
  col1 <- crayon::make_style("gray50")
  col2 <- col1$bold
  if(length(as.character(x)) == 1) x <- .uncollapse(x)
  cat(col2("<Lyrics string>\n  Format: "), a$format, col2("\n  Values: "),
      col1(.tabr_print4(x)), "\n", sep = "")
}

#' @export
summary.lyrics <- function(object, ...){
  a <- attributes(object)
  col1 <- crayon::make_style("gray50")
  col2 <- col1$bold
  cat(col2("<Lyrics string>\n  Timesteps: "), a$steps, " (",
      a$n_lyric, " ", paste0("lyric", ifelse(a$n_lyric == 1, "", "s")), ", ",
      a$n_pause, " ", paste0("pause", ifelse(a$n_pause == 1, "", "s"), ")"),
      col2("\n  Format: "), a$format, col2("\n  Values: "),
      col1(.tabr_print4(.uncollapse(as.character(object)))), "\n", sep = "")
}

.tabr_print4 <- function(x){
  col1 <- crayon::make_style("dodgerblue")$bold
  col2 <- crayon::make_style("orange2")
  x <- .uncollapse(x)
  x <- col1(x)
  x <- gsub("(\\.)", col2("\\1"), x)
  paste(x, collapse = " ")
}
