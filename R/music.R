#' Check music string validity
#'
#' Check whether a string is comprised exclusively of valid syntax for music
#' strings. A music object can be built from such a string. It combines a
#' noteworthy string and a note info string.
#'
#' With note info strings, you are required to enter something at every
#' timestep, even if it is only the duration. This makes sense because if you
#' do not enter something, there is simply no indication of a timestep.
#' A nice feature of music strings is that explicit timesteps are achieved
#' just by having notes present, allowing you to leave out durations entirely
#' when they repeat, inheriting them from the previous timestep where duration
#' was given explicitly. There is no need to enter the same number across
#' consecutive timesteps; the first will suffice and the rest are automatically
#' filled in for you when the object is constructed.
#'
#' \code{musical} returns a scalar logical result indicating whether all
#' timesteps contain exclusively valid entries.
#'
#' \code{as_music} can be used to coerce to the \code{music} class.
#' Coercion will fail if the string is not musical.
#' The \code{music} class has its own \code{print} and \code{summary} methods.
#'
#' When \code{accidentals}, \code{format}, or \code{tsig} are \code{NULL},
#' these settings are inferred from the musical string input.
#' When mixed formats are present, flats are the default for accidentals and
#' 4/4 common time is the default time signature.
#'
#' @param x character, a music string. May or may not have the 'music' class.
#' @param notes,info noteworthy and note info strings. For \code{as_music}, a
#' complete music string is assumed for \code{notes} when \code{info = NULL}.
#' @param lyrics optional \code{lyrics} object, attached to output as an
#' attribute.
#' @param accidentals \code{NULL} or character, represent accidentals,
#' \code{"flat"} or \code{"sharp"}.
#' @param tsig character, store the time signature as a music attribute.
#' Defaults to \code{"4/4"}.
#' @param format \code{NULL} or character, the timestep delimiter format,
#' \code{"space"} or \code{"vector"}.
#'
#' @return depends on the function
#' @export
#' @name music
#' @seealso \code{\link{note-checks}}, \code{\link{note-metadata}},
#' \code{\link{note-summaries}}, \code{\link{note-coerce}}
#'
#' @examples
#' # note durations inherit from previous timestep if missing
#' x <- "a#4] b_ c,x d''t8( e)( g_')- a4 c,e_,g, ce_g4. a~8 a1"
#' is_music(x)
#' musical(x)
#' x <- as_music(x)
#' is_music(x)
#' x
#'
#' summary(x)
#'
#' attributes(x)
#'
#' music_split(x)
#' music_notes(x)
#' music_info(x)
#' music_tsig(x)
#' music_lyrics(x)
#'
#' as_music(x, accidentals = "sharp")
#'
#' y <- lyrics_template(x)
#' y[3:8] <- strsplit("These are some song ly- rics", " ")[[1]]
#' y
#'
#' x <- as_music(x, lyrics = y)
#' x
#' music_lyrics(x)
musical <- function(x){
  if(is_music(x)) return(TRUE)
  .check_music_split(x, FALSE)
}

#' @export
#' @rdname music
as_music <- function(notes, info = NULL, lyrics = NULL, accidentals = NULL,
                     tsig = "4/4", format = NULL){
  null_args <- all(sapply(list(format, accidentals, lyrics), is.null))
  if(inherits(notes, "music") && null_args && music_tsig(notes) == tsig)
    return(notes)
  if(is.null(format)) format <- .infer_time_format(notes)
  if(is.null(info)){
    x <- .check_music_split(notes)
    notes <- x$notes
    info <- x$info
  } else {
    .check_noteworthy(notes)
    .check_noteinfo(info)
    .check_timesteps(notes, info)
  }
  .check_format_arg(format)
  .check_accidentals_arg(accidentals)
  .asmusic(notes, info, lyrics, accidentals, tsig, format)
}

.asmusic <- function(x, y, lyrics = NULL, accidentals = NULL, tsig = "4/4",
                     format = NULL){
  x <- .asnw(x, "tick", accidentals, "vector")
  y <- .asni(y, "vector")
  .check_timesteps(x, y)
  if(!is.null(lyrics)){
    if(!is_lyrics(lyrics))
      stop("`lyrics` must be a `lyrics`-class object.", call. = FALSE)
    .check_timesteps(x, lyrics, "lyrics")
  }
  ax <- c(attributes(x), list(tsig = tsig))
  x <- as.character(paste(x, y, sep = ""))
  if(is.null(format)) format <- "space"
  if(format == "space"){
    x <- paste(x, collapse = " ")
    ax$format <- "space-delimited time"
    if(!is.null(lyrics)){
      if(is_vector_time(lyrics)) lyrics <- as_space_time(lyrics)
    }
  } else {
    if(!is.null(lyrics)){
      if(is_space_time(lyrics)) lyrics <- as_vector_time(lyrics)
    }
  }
  ax$lyrics <- if(is.null(lyrics)) NA else lyrics
  attributes(x) <- ax[names(ax) != "class"]
  class(x) <- unique(c("music", class(x)))
  x
}

.check_timesteps <- function(x, y, ylab = "info"){
  nx <- n_steps(x)
  ny <- n_steps(y)
  if(nx > 1 & ny == 1) y <- rep(y, nx) else if(nx != ny)
    stop(paste0("`notes` and `", ylab, "` have unequal number of timesteps."),
         call. = FALSE)
}

#' @export
#' @rdname music
is_music <- function(x){
  inherits(x, "music")
}

#' @export
#' @rdname music
music_split <- function(x){
  lyrics <- if(is_music(x)) music_lyrics(x) else NULL
  x <- .check_music_split(x)
  c(x, list(lyrics = lyrics))
}

.check_music_split <- function(x, err = TRUE){
  format <- if(length(as.character(x) == 1)) "space" else "vector"
  x <- .uncollapse(x)
  e1 <- "Invalid notes or note info found."
  y <- gsub(";\\^\".*\"", "", x)
  if(any(grepl("[A-Zh-quvwyz]", y))){
    if(err) stop(e1, call. = FALSE) else return(FALSE)
  }
  if(err){
    .music_split(x, format)
  } else {
    x <- tryCatch(.music_split(x, format), error = function(e) NULL)
    !is.null(x)
  }
}

.music_split <- function(x, format){
  list(
    notes = .music_notes(x, format),
    info = .music_info(x, format),
    tsig = "4/4"
  )
}

.music_notes <- function(x, format){
  notes <- gsub("^([a-grs_#,'~]+).*", "\\1", x)
  as_noteworthy(notes, format = format)
}

.music_info <- function(x, format){
  info <- gsub("^[a-grs_#~,']+(.*)", "\\1", x)
  has_time <- grepl("^(t|)\\d+", info)
  if(!has_time[1])
    stop("First timestep must have a duration value.", call. = FALSE)
  if(any(!has_time)){
    x <- rep(NA, length(info))
    x[has_time] <- info_duration(info[has_time])
    for(i in seq_along(x)[-1]) if(is.na(x[i])) x[i] <- x[i - 1]
    info[!has_time] <- paste0(x[!has_time], info[!has_time])
  }
  as_noteinfo(info, format)
}

#' @export
#' @rdname music
music_notes <- function(x){
  music_split(x)$notes
}

#' @export
#' @rdname music
music_info <- function(x){
  music_split(x)$info
}

#' @export
#' @rdname music
music_tsig <- function(x){
  attr(x, "tsig")
}

#' @export
#' @rdname music
music_lyrics <- function(x){
  attr(x, "lyrics")
}

#' @export
print.music <- function(x, ...){
  a <- attributes(x)
  col1 <- crayon::make_style("gray50")
  col2 <- col1$bold
  if(length(as.character(x)) == 1) x <- .uncollapse(x)
  cat(col2("<Music string>\n  Format: "), a$format, col2("\n  Values: "),
      col1(.tabr_print3(x)), "\n", sep = "")
}

#' @export
summary.music <- function(object, ...){
  a <- attributes(object)
  col1 <- crayon::make_style("gray50")
  col2 <- col1$bold
  lyrics <- a$lyrics
  if(!is.na(lyrics) & length(lyrics) > 10)
    lyrics <- gsub("\\.{4}", "...", paste0(head(lyrics, 10), "..."))
  cat(col2("<Music string>\n  Timesteps: "), a$steps, " (",
      a$n_note, " ", paste0("note", ifelse(a$n_note == 1, "", "s")), ", ",
      a$n_chord, " ", paste0("chord", ifelse(a$n_chord == 1, "", "s"), ")"),
      col2("\n  Octaves: "), a$octave,
      col2("\n  Accidentals: "), a$accidentals,
      col2("\n  Time Signature: "), a$tsig,
      col2("\n  Lyrics: "), lyrics,
      col2("\n  Format: "), a$format, col2("\n  Values: "),
      col1(.tabr_print3(.uncollapse(as.character(object)))), "\n", sep = "")
}

.tabr_print3 <- function(x){
  col1 <- crayon::make_style("gray50")
  col2 <- crayon::make_style("dodgerblue")$bold
  col3 <- crayon::make_style("orange2")
  x <- .uncollapse(x)
  notes <- .music_notes(x, "vector")
  info <- .music_info(x, "vector")
  idx <- is_chord(notes)
  notes <- col2(notes)
  if(any(idx)) notes[idx] <- paste0("<", notes[idx], ">")
  idx <- grepl(";\\^\"", info)
  if(any(idx)){
    info[idx] <- purrr::map_chr(strsplit(info[idx], ";\\^\""), ~{
      x <- gsub("([-0-9t~x\\.\\(\\)]+|\\])", col3("\\1"), .x[1])
      paste(paste0(x, col3(";^")), col1(paste0("\"", .x[2])), sep = "")
    })
  }
  if(any(!idx)){
    info[!idx] <- gsub("([-0-9t~x\\.\\(\\)^]+|\\])", col3("\\1"), info[!idx])
  }
  paste(paste0(notes, info), collapse = " ")
}
