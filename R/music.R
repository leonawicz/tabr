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
#' \code{music} objects are primarily intended to represent an aggregation of a
#' \code{noteworthy} object and a \code{noteinfo}. You can optionally fold in a
#' \code{lyrics} object as well. However, for music data analysis, any
#' operations will involve first splitting the object into its component parts.
#' The value of this class is for the more efficient data entry it provides.
#'
#' When \code{accidentals} or \code{format} are \code{NULL},
#' these settings are inferred from the musical string input.
#' When mixed formats are present, flats are the default for accidentals.
#'
#' Other attributes are attached to a \code{music} object.
#' \code{key} uses the \code{tabr} syntax, e.g., \code{"c"}, \code{"b_"},
#' \code{"f#m"}, etc. \code{time} and \code{tempo} use the LilyPond string
#' format. For music programming and analysis, \code{key}, \code{time} and
#' \code{tempo} can most likely be ignored. They are primarily relevant when
#' rendering a music snippet directly from a \code{music} object with LilyPond.
#' These additional attributes provide more complete context for the rendered
#' sheet music.
#'
#' If you plan to render music snippets from a \code{music} object that you are
#' defining from a new character string, and the context you have in mind is a
#' stringed and fretted instrument like guitar, you can specify string numbers
#' at the end of each timestep with numbers following a semicolon delimiter.
#' This would still precede any \code{*} timestep multiplier number.
#' See examples.
#'
#' Note that if you convert a music object to a phrase object, you are changing
#' contexts. The phrase object is the simplest LilyPond-format music structure.
#' Coercion with \code{phrase} strips all attributes of a music object and
#' retains only notes, note info and string numbers.
#'
#' @param x character, a music string. May or may not have the 'music' class.
#' @param notes,info noteworthy and note info strings. For \code{as_music}, a
#' complete music string is assumed for \code{notes} when \code{info = NULL}.
#' @param lyrics optional \code{lyrics} object or \code{NA}, attached to output
#' as an attribute.
#' @param key character, store the key signature as a music attribute. Defaults
#' to \code{"c"}. See details.
#' @param time character, store the time signature as a music attribute.
#' Defaults to \code{"4/4"}. See details.
#' @param tempo character, defaults to \code{"2 = 60"}. See details.
#' @param accidentals \code{NULL} or character, represent accidentals,
#' \code{"flat"} or \code{"sharp"}.
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
#' music_key(x)
#' music_time(x)
#' music_tempo(x)
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
#'
#' # Starting string = 5: use \code{;5}. Carries over until an explicit change.
#' x <- "a,4;5*5 b,4- c4 cgc'e'~4 cgc'e'1 e'4;2 c';3 g;4 c;5 ce'1;51"
#' as_music(x)
musical <- function(x){
  if(is_music(x)) return(TRUE)
  .check_music_split(x, FALSE)
}

#' @export
#' @rdname music
as_music <- function(notes, info = NULL, lyrics = NA, key = "c", time = "4/4",
                     tempo = "2 = 60", accidentals = NULL, format = NULL){
  null_args <- is.null(accidentals) & is.null(format)
  if(inherits(notes, "music") && null_args && music_key(notes) == key &&
     music_time(notes) == time && music_tempo(notes) == tempo &&
     identical(music_lyrics(notes), lyrics)) return(notes)
  if(is.null(format)) format <- .infer_time_format(notes)
  if(is.null(info)){
    x <- .check_music_split(notes)
    notes <- x$notes
    info <- x$info
    s <- x$string
  } else {
    .check_noteworthy(notes)
    .check_noteinfo(info)
    .check_timesteps(notes, info)
    s <- NULL
  }
  .check_format_arg(format)
  .check_accidentals_arg(accidentals)
  .asmusic(notes, info, s, lyrics, key, time, tempo, accidentals, format)
}

.asmusic <- function(x, y, s, lyrics = NA, key = "c", time = "4/4",
                     tempo = "2 = 60", accidentals = NULL, format = NULL){
  x <- .asnw(x, "tick", accidentals, "vector")
  if(!is.null(s)) s <- .music_infer_strings(x, s)
  y <- .asni(y, "vector")
  .check_timesteps(x, y)
  if(!is.na(lyrics)){
    if(!is_lyrics(lyrics))
      stop("`lyrics` must be a `lyrics`-class object or NA.", call. = FALSE)
    .check_timesteps(x, lyrics, "lyrics")
  }
  ax <- c(attributes(x), list(key = key, time = time, tempo = tempo))
  x <- as.character(paste(x, y, sep = ""))
  if(is.null(format)) format <- "space"
  if(format == "space"){
    x <- paste(x, collapse = " ")
    ax$format <- "space-delimited time"
    if(!is.na(lyrics)){
      if(is_vector_time(lyrics)) lyrics <- as_space_time(lyrics)
    }
  } else {
    if(!is.na(lyrics)){
      if(is_space_time(lyrics)) lyrics <- as_vector_time(lyrics)
    }
  }
  ax$lyrics <- if(is.na(lyrics)) NA else lyrics
  if(!is.null(s)) ax$string <- s
  attributes(x) <- ax[names(ax) != "class"]
  class(x) <- unique(c("music", class(x)))
  x
}

.music_infer_strings <- function(x, s){
  size <- chord_size(x)
  rests <- note_is_rest(x)
  idx <- which(nchar(s) == 1 & size > 1)
  if(length(idx)) s[idx] <- purrr::map2_chr(as.integer(s[idx]), size[idx], ~{
    x <- paste(seq(.x, by = -1, length.out = .y), collapse = "")
    if(grepl("[-0]", x)) stop("Invalid string number < 1.", call. = FALSE)
    x
  })
  idx <- !rests & !is.na(s) & nchar(s) != 0
  if(any(idx)){
    if(any(nchar(s[idx]) != size[idx]))
      stop("Number of strings and notes must match at each non-rest timestep.",
           call. = FALSE)
  }
  s
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
  if(is_music(x)){
    lyrics <- music_lyrics(x)
    key = music_key(x)
    time = music_time(x)
    tempo = music_tempo(x)
  } else {
    lyrics <- NA
    key <- "c"
    time <- "4/4"
    tempo = "2 = 60"
  }
  x <- .check_music_split(x)
  c(x, list(lyrics = lyrics, key = key, time = time, tempo = tempo))
}

.check_music_split <- function(x, err = TRUE){
  format <- if(length(as.character(x) == 1)) "space" else "vector"
  s <- if(is_music(x)) attr(x, "string") else NA
  x <- .uncollapse(x)
  idx <- grep(";\\d+$", x)
  if(length(idx)){
    s <- rep(NA, length(x))
    s[idx] <- gsub(".*;(\\d+)$", "\\1", x[idx])
    s <- tidyr::fill(tibble::tibble(s), .data[["s"]])$s
    s <- ifelse(is.na(s), "", s)
    x <- gsub(";\\d+$", "", x)
  } else {
    if(is.null(s)) s <- NA
  }
  e1 <- "Invalid notes or note info found."
  y <- gsub(";\\^\".*\"", "", x)
  if(any(grepl("[A-Zh-quvwyz]", y))){
    if(err) stop(e1, call. = FALSE) else return(FALSE)
  }
  if(err){
    .music_split(x, s, format)
  } else {
    x <- tryCatch(.music_split(x, s, format), error = function(e) NULL)
    !is.null(x)
  }
}

.music_split <- function(x, s, format){
  x <- list(notes = .music_notes(x, format), info = .music_info(x, format))
  if(any(!is.na(s))) c(x, list(string = s)) else x
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
music_strings <- function(x){
  music_split(x)$string
}

#' @export
#' @rdname music
music_key <- function(x){
  attr(x, "key")
}

#' @export
#' @rdname music
music_time <- function(x){
  attr(x, "time")
}

#' @export
#' @rdname music
music_tempo <- function(x){
  attr(x, "tempo")
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
  s <- a$string
  has_s <- any(!is.na(s))
  if(has_s){
    s[s == ""] <- "."
    n <- length(s)
    s <- paste(head(s, 10), collapse = " ")
    if(n > 10) s <- gsub("\\.{4}", "...", paste0(s, "..."))
  }
  cat(col2("<Music string>\n  Timesteps: "), a$steps, " (",
      a$n_note, " ", paste0("note", ifelse(a$n_note == 1, "", "s")), ", ",
      a$n_chord, " ", paste0("chord", ifelse(a$n_chord == 1, "", "s"), ")"),
      col2("\n  Octaves: "), a$octave,
      col2("\n  Accidentals: "), a$accidentals,
      col2("\n  Key signature: "), a$key,
      col2("\n  Time signature: "), a$time,
      col2("\n  Tempo: "), a$tempo,
      col2("\n  Lyrics: "), lyrics,
      if(has_s) col2("\n  Strings: "), if(has_s) s,
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
