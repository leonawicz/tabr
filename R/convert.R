#' Music notation syntax converters
#'
#' Convert alternative representations of music notation to \code{tabr} syntax.
#'
#' These functions convert music notation from other data sources into the style
#' used by \code{tabr} for music analysis and sheet music transcription.
#'
#' Some sources do not offer as complete or explicit information in order to
#' make sheet music. However, what is available in those formats is converted
#' to the extent possible and available function arguments can allow the user
#' to add some additional specification. Different input syntax makes use of a
#' different syntax converter. Depending on the format, different arguments
#' may be available and/or required. The general wrapper function for all of
#' the available syntax converters is \code{to_tabr}. This function takes an
#' \code{id} argument for the appropriate converter function. See examples.
#'
#' For example, output from the \code{chorrrds} package that scrapes chord
#' information from the Cifraclub website only provides chords, not note for
#' note transcription data for any particular instrument. This means the result
#' of syntax conversion still yields only chords, which is fine for data
#' analysis but doesn't add anything useful for sheet music transcription.
#'
#' The input in this case also does not specify distinct pitches by assigning
#' octaves numbers to a chord's notes, not even the root note. It remains up to
#' the user if they want to apply the information. By default, every chord
#' starts in octave three. It is also ambiguous how the chord is played since
#' all that is provided is a generic chord symbol. By default a standard chord
#' is constructed if it can be determined.
#'
#' Setting \code{guitar = TRUE} switches
#' to using the \code{\link{guitarChords}} dataset to find matching guitar
#' chords using \code{\link{gc_info}}, which can be provided additional
#' arguments in a named list to \code{gc_args}. For guitar, this allows some
#' additional control over the actual structure of the chord, its shape and
#' position on the guitar neck. The options will never work perfectly for all
#' chords in \code{chords}, but at a minimum, typical default component pitches
#' will be determined and returned in \code{tabr} notation style.
#'
#' @param chords character vector of chords output from the \code{chorrrds}
#' package.
#' @param key key signature, used to enforce consistent use of flats or sharps.
#' @param guitar logical, attempt to match input chords to known guitar chords
#' in \code{\link{guitarChords}}. Otherwise by default standard piano chords of
#' consecutive pitches covering minimum pitch range are returned.
#' @param gc_args named list of additional arguments passed to
#' \code{\link{gc_info}}, used when \code{guitar = TRUE}.
#' @param ... arguments passed to the function matched by \code{id}.
#'
#' @return a noteworthy string
#' @export
#'
#' @examples
#' chords <- c("Bb", "Bbm", "Bbm7", "Bbm7(b5)", "Bb7(#5)/G", "Bb7(#5)/Ab")
#' from_chorrrds(chords)
#' to_tabr(id = "chorrrds", chords = chords)
#'
#' from_chorrrds(chords, guitar = TRUE)
#' to_tabr(id = "chorrrds", chords = chords, guitar = TRUE)
to_tabr <- function(id, ...){
  x <- paste0("from_", id)
  f <- tryCatch(utils::getFromNamespace(x, "tabr"), error = function(e) NULL)
  if(is.null(f)){
    stop(paste0("Function `tabr::", x, "` not found."), call. = FALSE)
  }
  f(...)
}

#' @export
#' @rdname to_tabr
from_chorrrds <- function(chords, key = "c", guitar = FALSE, gc_args = list()){
  key <- .process_key(key)
  x <- tolower(gsub("\\(|\\)", "", gsub("b", "_", chords)))
  alt_bass <- .get_alt_bass(x)
  x <- gsub("/.*", "", x)
  x <- chord_name_split(x)
  xfuns <- paste0("x", gsub("#", "s", gsub("_", "b", x$mod)))
  xfuns <- lapply(xfuns, utils::getFromNamespace, ns = "tabr")
  if(guitar){
    x <- paste0(x$root, x$mod)
    x <- purrr::map(x, ~do.call(gc_info, c(list(name = .x), gc_args))$notes[1])
  } else {
    x <- purrr::map2_chr(x$root, xfuns, ~.y(.x, key))
  }
  if(any(!is.na(alt_bass))){
    if(guitar){
      warning("Alternate bass note detected, but ignored when `guitar = TRUE`.",
              call. = FALSE)
    } else {
      idx <- which(!is.na(alt_bass))
      x[idx] <- .set_alt_bass(x[idx], alt_bass[idx], key)
    }
  }
  acc <- if(key_is_sharp(key)) "sharp" else "flat"
  .asnw(x, "tick", acc, "space")
}

.process_key <- function(key){
  key <- tolower(key)
  minor <- grepl("m", key)
  key <- gsub("m|M", "", key)
  if(nchar(key) == 2) key <- gsub("([a-g])b", "\\1_")
  if(minor) key <- paste0(key, "m")
  .keycheck(key)
  key
}

.get_alt_bass <- function(x){
  x <- strsplit(x, "/")
  alt_bass <- rep(NA, length(x))
  idx <- sapply(x, function(x) length(x) == 2)
  alt_bass[idx] <- sapply(x[idx], "[", 2)
  alt_bass
}

.set_alt_bass <-function(x, alt_bass, key){
  purrr::map2_chr(x, alt_bass, ~{
    pitches <- .split_chords(.x)
    notes <- .pitch_to_note(pitches)
    n <- length(notes)
    y <- .y
    drop_top <- notes[n] == .pitch_to_note(.y)
    acc <- if(key_is_sharp(key)) "sharp" else "flat"
    while(.pitch_semitones(y, accidentals = acc) > .pitch_semitones(
      pitches[1], accidentals = acc)){
      y <- transpose(y, -12, key = key)
    }
    pitches <- if(drop_top) c(y, pitches[-n]) else c(y, pitches)
    paste(pitches, collapse = "")
  })
}
