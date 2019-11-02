#' Note info helpers
#'
#' Functions for working with note info strings.
#'
#' @param x character, note info string normally accompanying a noteworthy
#' string for building phrase objects. \code{x} may also be a phrase object.
#' See examples.
#'
#' @return character
#' @export
#' @name noteinfo
#' @seealso \code{\link{valid-noteinfo}}
#'
#' @examples
#' a <- notate("8x", "Start here")
#' notes <- "a, b, c d e f g# a r ac'e' a c' e' c' r*3 ac'e'~ ac'e'"
#' info <- paste(a, "8x 8] 16 4.. 16- 16 2^ 2 4. 8( 4)( 4) 8*4 1 1")
#' x <- phrase(notes, info)
#'
#' data.frame(
#'   duration = info_duration(x),
#'   slur_on = info_slur_on(x),
#'   slur_off = info_slur_off(x),
#'   slide = info_slide(x),
#'   dotted = info_dotted(x),
#'   dotted1 = info_single_dotted(x),
#'   dotted2 = info_double_dotted(x),
#'   annotation = info_annotation(x)
#' )
#'
info_duration <- function(x){
  .asni(gsub("^([t0-9\\.]+).*", "\\1", .parse_info(x)))
}

#' @export
#' @rdname noteinfo
info_slur_on <- function(x){
  grepl("\\(", .parse_info(x))
}

#' @export
#' @rdname noteinfo
info_slur_off <- function(x){
  grepl("\\)", .parse_info(x))
}

#' @export
#' @rdname noteinfo
info_slide <- function(x){
  grepl("[-]", .parse_info(x))
}

#' @export
#' @rdname noteinfo
info_dotted <- function(x){
  grepl("\\.{1,2}", .parse_info(x))
}

#' @export
#' @rdname noteinfo
info_single_dotted <- function(x){
  info_dotted(x) & !info_double_dotted(x)
}

#' @export
#' @rdname noteinfo
info_double_dotted <- function(x){
  grepl("\\.{2}", .parse_info(x))
}

#' @export
#' @rdname noteinfo
info_annotation <- function(x){
  if(inherits(x, "phrase")){
    x <- phrase_info(x, FALSE)
  } else if(inherits(x, "music")){
    x <- .uncollapse(music_info(x))
  } else {
    x <- .uncollapse(x)
  }
  y <- rep(NA_character_, length(x))
  idx <- grep(";\\^\".*\"", x)
  if(length(idx)) y[idx] <- gsub(".*;\\^\"(.*)\"", "\\1", x[idx])
  gsub("_", " ", y)
}

.parse_info <- function(x){
  if(inherits(x, "phrase")){
    phrase_info(x, FALSE, FALSE)
  } else if(inherits(x, "music")){
    .uncollapse(music_info(x))
  } else {
    .uncollapse(x)
  }
}

#' Check note info validity
#'
#' Check whether a note info string is comprised exclusively of valid note info
#' syntax.
#' \code{noteinfo} returns a scalar logical result indicating whether the
#' entire set contains exclusively valid entries.
#'
#' \code{as_noteinfo} can be used to coerce to the \code{noteinfo} class.
#' Coercion will fail if the string is has any syntax that is not valid for
#' note info.
#' Using the \code{noteinfo} class is generally not needed by the user during
#' an interactive session, but is available and offers its own \code{print} and
#' \code{summary} methods for note info strings.
#' The class is often used by other functions, and functions that output a
#' note info string attach the \code{noteinfo} class.
#'
#' When \code{format = NULL}, the timestep delimiter format is inferred
#' from the note info string input. When unclear, such as with phrase objects,
#' the default is space-delimited time.
#'
#' @param x character, a note info string.
#' @param format \code{NULL} or character, the timestep delimiter format,
#' \code{"space"} or \code{"vector"}.
#' @param na.rm remove \code{NA}s.
#'
#' @return depends on the function
#' @export
#' @name valid-noteinfo
#' @seealso \code{\link{noteinfo}}, \code{\link{valid-notes}}
#'
#' @examples
#' a <- notate("8x", "Start here")
#' x <- paste(a, "8] 8] 16 4.. 16- 16 2^ 2 4. 8( 4)( 4) 8*4 1 1")
#'
#' informable(x) # is it of 'noteinfo' class; a validity check for any string
#' x <- as_noteinfo(x) # coerce to 'noteinfo' class
#' is_noteinfo(x) # check for 'noteinfo' class
#' x
#'
#' summary(x)
informable <- function(x, na.rm = FALSE){
  if(is_noteinfo(x)) return(TRUE)
  if(na.rm){
    x <- x[!is.na(x)]
    if(!is.character(x)) x <- as.character(x)
  }
  x <- .uncollapse(x)
  if(!length(x) | any(x == "")) return(FALSE)
  x <- gsub(";\\^\".*\"", "", x)
  durations <- gsub("^(t|)(\\d+|).*", "\\2", x)
  if(!all(durations %in% c(1, 2, 4, 8, 16, 32))) return(FALSE)
  x <- gsub("t\\d+|[123468\\(\\)\\.\\^x]+|\\[|\\]|-", "", x)
  if(all(x == "")) TRUE else FALSE
}

.asni <- function(x, format = NULL){
  if(is.null(format)) format <- .infer_time_format(x)
  format <- switch(format, space = "space-delimited time",
                   vector = "vectorized time")
  x <- .uncollapse(x)
  steps <- length(x)
  if(format == "space-delimited time") x <- paste(x, collapse = " ")
  attributes(x) <- list(steps = steps, format = format)
  class(x) <- unique(c("noteinfo", class(x)))
  x
}

#' @export
#' @rdname valid-noteinfo
as_noteinfo <- function(x, format = NULL){
  if(inherits(x, "noteinfo") & is.null(format)) return(x)
  .check_noteinfo(x)
  .check_format_arg(format)
  .asni(x, format)
}

#' @export
#' @rdname valid-noteinfo
is_noteinfo <- function(x){
  "noteinfo" %in% class(x)
}

.check_noteinfo <- function(x, na.rm = FALSE){
  if(!informable(x, na.rm)) stop("Invalid note info found.", call. = FALSE)
}

#' @export
print.noteinfo <- function(x, ...){
  a <- attributes(x)
  col1 <- crayon::make_style("gray50")
  col2 <- col1$bold
  if(length(as.character(x)) == 1) x <- .uncollapse(x)
  cat(col2("<Note info string>\n  Format: "), a$format, col2("\n  Values: "),
      col1(.tabr_print2(x)), "\n", sep = "")
}

#' @export
summary.noteinfo <- function(object, ...){
  a <- attributes(object)
  col1 <- crayon::make_style("gray50")
  col2 <- col1$bold
  cat(col2("<Note info string>\n  Timesteps: "), a$steps,
      col2("\n  Format: "), a$format, col2("\n  Values: "),
      col1(.tabr_print2(.uncollapse(as.character(object)))), "\n", sep = "")
}

.tabr_print2 <- function(x){
  durations <- crayon::make_style("dodgerblue")$bold
  other_info <- crayon::make_style("orange2")
  x <- gsub("^(t\\d+|\\d+|\\d+\\.+)", durations("\\1"), x)
  x <- gsub("(\\(|\\)|;\\^|\\^|x|-|\\])", other_info("\\1"), x)
  paste(x, collapse = " ")
}
