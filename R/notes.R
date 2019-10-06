#' Noteworthy string metadata
#'
#' Inspect metadata for noteworthy strings.
#'
#' Returned object depends on the nature of the function. It can be integers,
#' logical, character. Results can be a vector of equal length of a single
#' value summary.
#' \code{distinct_notes} and \code{distinct_pitches} filter a noteworthy string
#' to its unique elements, respectively. These functions return another
#' noteworthy string.
#'
#' The \code{n_*} functions give summary totals of the number of timesteps,
#' number of individual note (non-chord) timesteps, number of chord time
#' steps, and the number of distinct octaves present across timesteps.
#' Use the \code{tally_*} and \code{distinct_*} functions specifically for
#' summaries of unique elements.
#'
#' \code{*_span} functions are just the size of a range, e.g.,
#' \code{semitone_range} and \code{semitone_span}.
#'
#' Functions pertaining to type or format of a noteworthy string provide
#' information on how a particular string is defined, e.g. \code{time_format}.
#'
#' @param notes character, a noteworthy string, space-delimited or vector of
#' individual entries.
#'
#' @return integer or character
#' @export
#' @name note-metadata
#'
#' @examples
#' x <- "e_2 a_, c#f#a#"
#' n_steps(x)
#' n_notes(x)
#' n_chords(x)
#' n_octaves(x)
#'
#' tally_notes(x)
#' tally_pitches(x)
#' tally_octaves(x)
#' distinct_notes(x)
#' distinct_pitches(x)
#' distinct_octaves(x)
#'
#' pitch_range(x)
#' semitone_range(x)
#' semitone_span(x)
#' octave_range(x)
#' octave_span(x)
#'
#' octave_type(x)
#' accidental_type(x)
#' time_format(x)
#' is_space_time(x)
#' is_vector_time(x)
n_steps <- function(notes){
  attr(as_noteworthy(notes), "steps")
}

#' @export
#' @rdname note-metadata
n_notes <- function(notes){
  attr(as_noteworthy(notes), "n_note")
}

#' @export
#' @rdname note-metadata
n_chords <- function(notes){
  attr(as_noteworthy(notes), "n_chord")
}

#' @export
#' @rdname note-metadata
n_octaves <- function(notes){
  length(distinct_octaves(notes))
}

#' @export
#' @rdname note-metadata
tally_notes <- function(notes){
  .check_noteworthy(notes)
  x <- unlist(lapply(.uncollapse(notes), .split_chord))
  x <- sapply(x, .pitch_to_note, USE.NAMES = FALSE)
  x <- as.data.frame(table(x), stringsAsFactors = FALSE) %>%
    dplyr::as_tibble() %>%
    stats::setNames(c("note", "n"))
  ord <- order(sapply(x$note,
                      function(x) pitch_interval("c", x, ignore_octave = TRUE)))
  x[ord, ]
}

#' @export
#' @rdname note-metadata
tally_pitches <- function(notes){
  .check_noteworthy(notes)
  x <- unlist(lapply(.uncollapse(notes), .split_chord))
  x <- as.data.frame(table(x), stringsAsFactors = FALSE) %>%
    dplyr::as_tibble() %>%
    stats::setNames(c("pitch", "n"))
  ord <- order(sapply(x$pitch, function(x) pitch_interval("c", x)))
  x[ord, ]
}

#' @export
#' @rdname note-metadata
tally_octaves <- function(notes){
  .check_noteworthy(notes)
  x <- unlist(lapply(.uncollapse(notes), .split_chord))
  x <- sapply(x, .pitch_to_octave, USE.NAMES = FALSE)
  as.data.frame(table(x), stringsAsFactors = FALSE) %>%
    dplyr::as_tibble() %>%
    stats::setNames(c("octave", "n")) %>%
    dplyr::mutate(octave = as.integer(.data[["octave"]]))
}

#' @export
#' @rdname note-metadata
distinct_notes <- function(notes){
  x <- tally_notes(notes)$note
  if(time_format(notes) == "space-delimited time") x <- paste(x, collapse = " ")
  .asnw(x)
}

#' @export
#' @rdname note-metadata
distinct_pitches <- function(notes){
  x <- tally_pitches(notes)$pitch
  if(time_format(notes) == "space-delimited time") x <- paste(x, collapse = " ")
  .asnw(x)
}

#' @export
#' @rdname note-metadata
distinct_octaves <- function(notes){
  tally_octaves(notes)$octave
}

#' @export
#' @rdname note-metadata
pitch_range <- function(notes){
  x <- .uncollapse(distinct_pitches(notes))
  if(length(x) == 1) c(x, x) else c(x[1], utils::tail(x, 1))
}

#' @export
#' @rdname note-metadata
semitone_range <- function(notes){
  pitch_semitones(pitch_range(notes))
}

#' @export
#' @rdname note-metadata
semitone_span <- function(notes){
  diff(semitone_range(notes))
}

#' @export
#' @rdname note-metadata
octave_range <- function(notes){
  as.integer(range(distinct_octaves(notes)))
}

#' @export
#' @rdname note-metadata
octave_span <- function(notes){
  diff(octave_range(notes))
}

#' @export
#' @rdname note-metadata
octave_type <- function(notes){
  attr(as_noteworthy(notes), "octave")
}

#' @export
#' @rdname note-metadata
accidental_type <- function(notes){
  attr(as_noteworthy(notes), "accidentals")
}

#' @export
#' @rdname note-metadata
time_format <- function(notes){
  attr(as_noteworthy(notes), "format")
}

#' @export
#' @rdname note-metadata
is_space_time <- function(notes){
  time_format(notes) == "space-delimited time"
}

#' @export
#' @rdname note-metadata
is_vector_time <- function(notes){
  time_format(notes) == "vectorized time"
}

#' Inspect or coerce noteworthy string formats
#'
#' Helper functions for inspecting and setting formatting attributes of
#' noteworthy strings including representation of timesteps, octaves and
#' accidentals.
#'
#' The \code{note_is_*} functions strictly allow individual notes, not chords.
#' The other functions listed here accept any noteworthy string including those
#' containing chords.
#'
#' For \code{sharpen_flat} and \code{flatten_sharp}, sharpening flats and
#' flattening sharps refer to inverting their respective notation,
#' not to raising or lowering a flatted or sharped note by one semitone.
#' For the latter, use \code{naturalize}, which removes flat and/or sharp
#' notation from a string.
#' \code{note_set_key} is used for coercing a noteworthy string to a specific
#' and consistent notation for accidentals based on a key signature.
#' This is a wrapper around \code{sharpen_flat} and \code{flatten_sharp}.
#' \code{as_tick_octaves}, \code{as_integer_octaves}, \code{as_space_time} and
#' \code{as_vector_time} similarly affect octave and timestep format.
#' For simultaneous control over the representation of timesteps, octave
#' numbering and accidentals, all three are available as arguments to
#' \code{\link{as_noteworthy}}.
#'
#' @param notes character, a noteworthy string, space-delimited or vector of
#' individual entries.
#' @param type character, type of note to naturalize.
#' @param ignore_octave logical, strip any octave notation that may be present,
#' returning only the basic notes without explicit pitch.
#' @param key character, key signature to coerce any accidentals to the
#' appropriate form for the key. May also specify \code{"sharp"} or
#' \code{"flat"}.
#'
#' @return character
#' @export
#'
#' @examples
#' x <- "a_2 a a#'"
#' note_is_natural(x)
#' note_is_accidental(x)
#' note_is_flat(x)
#' note_is_sharp(x)
#'
#' x <- "e_2 a_, b_, c#f#a# c#'f#'a#''"
#' note_set_key(x, "f")
#' note_set_key(x, "g")
#' as_tick_octaves(x)
#' as_integer_octaves(x)
#' y <- as_vector_time(x)
#' is_vector_time(y)
#' is_space_time(as_space_time(y))
#'
#' naturalize(x)
#' naturalize(x, "sharp")
#' sharpen_flat(x)
#' flatten_sharp(x)
#' pretty_notes(x)
note_is_natural <- function(notes){
  .check_note(notes)
  sapply(.uncollapse(notes), .pitch_natural, USE.NAMES = FALSE)
}

#' @export
#' @rdname note_is_natural
note_is_accidental <- function(notes){
  .check_note(notes)
  sapply(.uncollapse(notes), .pitch_accidental, USE.NAMES = FALSE)
}

#' @export
#' @rdname note_is_natural
note_is_flat <- function(notes){
  .check_note(notes)
  sapply(.uncollapse(notes), .pitch_flat, USE.NAMES = FALSE)
}

#' @export
#' @rdname note_is_natural
note_is_sharp <- function(notes){
  .check_note(notes)
  sapply(.uncollapse(notes), .pitch_sharp, USE.NAMES = FALSE)
}

#' @export
#' @rdname note_is_natural
naturalize <- function(notes, type = c("both", "flat", "sharp"),
                       ignore_octave = FALSE){
  .check_noteworthy(notes)
  type <- match.arg(type)
  pat <- switch(type, both = "_|#", flat = "_", sharp = "#")
  x <- gsub(pat, "", notes)
  if(ignore_octave) x <- .pitch_to_note(x)
  .asnw(x)
}

#' @export
#' @rdname note_is_natural
sharpen_flat <- function(notes, ignore_octave = FALSE){
  .check_noteworthy(notes)
  x <- if(length(notes) > 1) paste0(notes, collapse = " ") else notes
  x <- transpose(x, 0, key = "g",
                 style = ifelse(ignore_octave, "strip", "default"))
  if(length(notes) > 1) x <- .uncollapse(x)
  .asnw(x)
}

#' @export
#' @rdname note_is_natural
flatten_sharp <- function(notes, ignore_octave = FALSE){
  .check_noteworthy(notes)
  x <- if(length(notes) > 1) paste0(notes, collapse = " ") else notes
  x <- transpose(x, 0, key = "f",
                 style = ifelse(ignore_octave, "strip", "default"))
  if(length(notes) > 1) x <- .uncollapse(x)
  .asnw(x)
}

#' @export
#' @rdname note_is_natural
note_set_key <- function(notes, key = "c"){
  notes <- as_noteworthy(notes)
  if(key == "flat") return(flatten_sharp(notes))
  if(key == "sharp") return(sharpen_flat(notes))
  .keycheck(key)
  if(key_is_natural(key)) return(notes)
  Recall(notes, .keydata$sf[.keydata$key == key])
}

#' @export
#' @rdname note_is_natural
as_tick_octaves <- function(notes){
  as_noteworthy(notes, octaves = "tick")
}

#' @export
#' @rdname note_is_natural
as_integer_octaves <- function(notes){
  as_noteworthy(notes, octaves = "integer")
}

#' @export
#' @rdname note_is_natural
as_space_time <- function(notes){
  as_noteworthy(notes, format = "space")
}

#' @export
#' @rdname note_is_natural
as_vector_time <- function(notes){
  as_noteworthy(notes, format = "vector")
}

#' @export
#' @rdname note_is_natural
pretty_notes <- function(notes, ignore_octave = TRUE){
  .check_noteworthy(notes)
  if(ignore_octave) notes <- .pitch_to_note(notes)
  gsub("_", "b", toupper(notes))
}

#' Slice, rotate, shift and arpeggiate notes
#'
#' Helper functions for indexing and moving notes within noteworthy strings.
#'
#' \code{note_slice} subsets the timesteps of a noteworthy string by integer
#' index or logical vector of length equal to the number of timesteps.
#'
#' \code{note_rotate} simply rotates anything space-delimited or vectorized in
#' place. It allows chords. Octave numbering is ignored if present.
#'
#' For \code{note_shift} the entire sequence is shifted up or down, as if
#' inverting a broken chord.
#' In this case \code{notes} is strictly interpreted and may not include chords.
#' Octave numbering applies, though large multi-octave gaps will be condensed
#' in the process.
#' Given the context of \code{note_shift}, the \code{notes} sequence should be
#' ordered by increasing pitch.
#' If it is not, ordering will be forced with each inversion during the
#' \code{n} shifts.
#'
#' \code{note_arpeggiate} also allows notes only. It is similar to
#' \code{note_shift}, except that instead of a moving window,
#' it grows from the original set of notes by \code{n} in the direction of the
#' sign of \code{n}.
#'
#' @param notes character, a noteworthy string, space-delimited or vector of
#' individual entries.
#' @param n integer, degree of rotation.
#' @param ... For \code{note_slice}, an integer or logical vector. See details.
#' For \code{note_arpeggiate}, additional arguments to \code{transpose},
#' specifically \code{key} and \code{style}.
#'
#' @return character
#' @export
#'
#' @examples
#' x <- "e_2 a_, c#f#a#"
#' note_slice(x, 2:3)
#' note_slice(x, c(FALSE, TRUE, TRUE))
#'
#' note_rotate(x, 1)
#'
#' note_shift("c e g", 1)
#' note_shift("c e g", -4)
#'
#' note_arpeggiate("c e g", 5)
#' note_arpeggiate("c e g", -5)
note_slice <- function(notes, ...){
  .check_noteworthy(notes)
  x <- .uncollapse(notes)
  idx <- c(...)
  if(!is.logical(idx) & !is.numeric(idx))
    stop("Must provide integer or logical vector index to slice `notes`.",
         call. = FALSE)
  if(is.logical(idx) & length(idx) != length(x))
    stop(paste("Logical vector must be same length as the number of timesteps",
               "in `notes`."), call. = FALSE)
  if(is.numeric(idx)) idx <- as.integer(idx)
  x <- x[idx]
  x <- x[!is.na(x)]
  if(!length(x)) stop("Index out of bounds.", call. = FALSE)
  if(length(notes) == 1) x <- paste0(x, collapse = " ")
  .asnw(x)
}

#' @export
#' @rdname note_slice
note_rotate <- function(notes, n = 0){
  .check_noteworthy(notes)
  x <- .uncollapse(notes)
  n <- n %% length(x)
  if(n == 0) return(notes)
  style <- if(any(grepl(",|'", notes))) "tick" else "integer"
  x <- x[c((n + 1):length(x), 1:n)]
  if(style == "tick") x <- .octave_to_tick(x)
  if(length(notes) == 1) x <- paste0(x, collapse = " ")
  .asnw(x)
}

#' @export
#' @rdname note_slice
note_shift <- function(notes, n = 0){
  .check_note(notes)
  if(n == 0) return(notes)
  x <- .uncollapse(notes)
  nx <- length(x)
  style <- if(any(grepl(",|'", notes))) "tick" else "integer"
  if(nx == 1) return(transpose(x, 12 * n, style = style))
  idx <- (0:n)[-1]
  f <- function(x) if(x == 3) "" else x
  if(n > 0){
    for(i in idx){
      o <- .pitch_to_octave(x[nx])
      x[1] <- paste0(.pitch_to_note(x[1]), f(o))
      if(pitch_interval(x[1], x[nx]) > 0)
        x[1] <- paste0(.pitch_to_note(x[1]), f(o + 1))
      x <- x[c(2:(nx), 1)]
    }
  } else {
    for(i in idx){
      o <- .pitch_to_octave(x[1])
      x[nx] <- paste0(.pitch_to_note(x[nx]), f(o))
      if(pitch_interval(x[nx], x[1]) < 0)
        x[nx] <- paste0(.pitch_to_note(x[nx]), f(o - 1))
      x <- x[c(nx, 1:(nx - 1))]
    }
  }
  if(style == "tick") x <- .octave_to_tick(x)
  if(length(notes) == 1) x <- paste0(x, collapse = " ")
  .asnw(x)
}

#' @export
#' @rdname note_slice
note_arpeggiate <- function(notes, n = 0, ...){
  .check_note(notes)
  if(n == 0) return(notes)
  x <- .uncollapse(notes)
  sharp <- !grepl("_", paste(x, collapse = " ")) &
    grepl("#", paste(x, collapse = " "))
  if(!is.null(list(...)$key)){
    k <- .keydata[.keydata$key == list(...)$key, ]
    if(!is.na(k$sf)) sharp <- k$sf == "sharp"
  }
  nx <- length(x)
  style <- if(any(grepl(",|'", notes))) "tick" else "integer"
  s <- sign(n) * seq(12, 12 * (abs(n) %/% nx + abs(n) %% nx), by = 12)
  if(n > 0){
    x <- c(x, sapply(s,
                     function(i) transpose(paste(x, collapse = " "), i, ...)))
    x <- paste(x, collapse = " ")
    x <- if(sharp) sharpen_flat(x) else flatten_sharp(x)
    x <- .uncollapse(x)[1:(nx + n)]
  } else {
    x <- c(sapply(rev(s),
                  function(i) transpose(paste(x, collapse = " "), i, ...)), x)
    x <- paste(x, collapse = " ")
    x <- if(sharp) sharpen_flat(x) else flatten_sharp(x)
    x <- utils::tail(.uncollapse(x), nx - n)
  }
  if(style == "tick") x <- .octave_to_tick(x)
  if(length(notes) == 1) x <- paste0(x, collapse = " ")
  .asnw(x)
}

#' Check note and chord validity
#'
#' Check whether a string is comprised exclusively of valid note and/or chord
#' substring syntax.
#' \code{is_note} and \code{is_chord} are vectorized and their positive results
#' are mutually exclusive.
#' \code{noteworthy} is also vectorized and performs both checks, but it
#' returns a scalar logical result indicating whether the entire set contains
#' exclusively valid entries.
#'
#' \code{as_noteworthy} can be used to coerce to the \code{noteworthy} class.
#' Coercion will fail if the string is not noteworthy.
#' Using the \code{noteworthy} class is generally not needed by the user during
#' an interactive session, but is available and offers its own \code{print} and
#' \code{summary} methods for noteworthy strings.
#' It is more likely to be used by other functions and functions that output a
#' noteworthy string generally attach the noteworthy class.
#'
#' \code{is_diatonic} performs a vectorized logical check on a
#' \code{noteworthy} string for all notes and chords.
#' To check strictly notes or chords, see \code{\link{note_in_scale}} and
#' \code{\link{chord_is_diatonic}}.
#'
#' @param x character, space-delimited entries or a vector of single,
#' non-delimited entries.
#' @param key character, key signature.
#' @param format \code{NULL} or character (\code{"space"} or \code{"vector"}),
#' the timestep delimiter format.
#' @param octaves \code{NULL} or character (\code{"tick"} or \code{"integer"}),
#' the octave representation.
#' @param accidentals \code{NULL} or character (\code{"flat"} or
#' \code{"sharp"}), the accidentals representation.
#'
#' @return logical
#' @export
#' @name valid-notes
#' @seealso \code{\link{note_in_scale}}, \code{\link{chord_is_diatonic}}
#'
#' @examples
#' x <- "a# b_ c, d'' e3 g_4 A m c2e_2g2 cegh"
#' data.frame(
#'   x = strsplit(x, " ")[[1]],
#'   note = is_note(x),
#'   chord = is_chord(x),
#'   either = noteworthy(x))
#'
#' is_diatonic("ace ac#e d e_", "c")
#'
#' x <- "a# b_ c,~ c, d'' e3 g_4 c2e_2g2"
#' x <- as_noteworthy(x)
#' x
#'
#' summary(x)
#'
#' x <- as_noteworthy(x, format = "vector", octaves = "integer",
#'                    accidentals = "flat")
#' x
#'
#' summary(x)
is_note <- function(x){
  x <- .uncollapse(x)
  y1 <- grepl("[a-grs]", x) & !grepl("[h-qt-zA-Z]", x)
  y2 <- gsub("\\d|,|'|_|#|~|\\*", "", x)
  y1 & nchar(y2) == 1 & y2 == substr(x, 1, 1) & !grepl("(r|s)\\d", x)
}

#' @export
#' @rdname valid-notes
is_chord <- function(x){
  x <- .uncollapse(x)
  y <- sapply(x, function(x){
    length(tryCatch(.split_chord(x), error = function(e) NA)) > 1
  }, USE.NAMES = FALSE)
  if(!any(y)) return(y)
  idx <- which(y)
  y[idx] <- sapply(x[idx], function(x) all(is_note(.split_chord(x))),
                   USE.NAMES = FALSE)
  y
}

#' @export
#' @rdname valid-notes
noteworthy <- function(x){
  if("noteworthy" %in% class(x)) return(TRUE)
  all(is_note(x) | is_chord(x))
}

#' @export
#' @rdname valid-notes
is_diatonic <- function(x, key = "c"){
  .check_noteworthy(x)
  s <- scale_diatonic(key, ignore_octave = TRUE)
  x <- .uncollapse(x)
  sapply(x, function(x) all(.pitch_to_note(.split_chord(x)) %in% s),
         USE.NAMES = FALSE)
}

.check_note <- function(x) if(any(!is_note(x)))
  stop("Invalid note found.", call. = FALSE)

.check_chord <- function(x) if(any(!is_chord(x)))
  stop("Invalid chord found.", call. = FALSE)

.check_noteworthy <- function(x) if(!noteworthy(x))
  stop("Invalid notes or chords found.", call. = FALSE)

.asnw <- function(x, format = NULL){
  n <- length(x)
  if(is.null(format)){
    format <- if(n == 1) "space-delimited time" else "vectorized time"
  }
  x <- .uncollapse(x)
  steps <- length(x)
  nnote <- as.integer(sum(is_note(x)))
  nchord <- as.integer(sum(is_chord(x)))
  flat <- any(.pitch_flat(x))
  sharp <- any(.pitch_sharp(x))
  if(flat & sharp){
    a <- "both/ambiguous"
  } else if(flat){
    a <- "flat"
  } else if(sharp){
    a <- "sharp"
  } else {
    a <- "none/unknown"
  }
  tick <- any(grepl(",|'", x))
  int <- any(grepl("\\d", x))
  if(tick & int){
    o <- "ambiguous"
  } else if(tick){
    o <- "tick"
  } else if(int){
    o <- "integer"
  } else {
    o <- "unknown"
  }
  if(format == "space-delimited time") x <- paste(x, collapse = " ")
  attributes(x) <- list(steps = steps, n_note = nnote, n_chord = nchord,
                        octave = o, accidentals = a, format = format)
  class(x) <- unique(c("noteworthy", class(x)))
  x
}

#' @export
#' @rdname valid-notes
as_noteworthy <- function(x, format = NULL, octaves = NULL, accidentals = NULL){
  null_args <- all(sapply(list(format, octaves, accidentals), is.null))
  if("noteworthy" %in% class(x) & null_args) return(x)
  .check_noteworthy(x)
  if(!is.null(format)){
    if(format %in% c("space", "vector")){
      format <- switch(format, space = "space-delimited time",
                       vector = "vectorized time")
    } else {
      stop("`format` must be 'space' or 'vector' if not NULL.", call. = FALSE)
    }
  }
  if(!is.null(octaves)){
    if(octaves %in% c("tick", "integer")){
      x <- if(octaves == "tick") .octave_to_tick(x) else .octave_to_int(x)
    } else {
      stop("`octaves` must be 'tick' or 'integer' if not NULL.", call. = FALSE)
    }
  }
  if(!is.null(accidentals)){
    if(!accidentals %in% c("flat", "sharp"))
      stop("`accidentals` must be 'flat' or 'sharp' if not NULL.",
           call. = FALSE)
    x <- if(accidentals == "flat") flatten_sharp(x) else sharpen_flat(x)
  }
  .asnw(x, format)
}

#' @export
print.noteworthy <- function(x, ...){
  col1 <- crayon::make_style("gray50")$bold
  if(length(x) == 1){
    format <- "space-delimited time"
    x <- .uncollapse(x)
  } else {
    format <- "vectorized time"
  }
  cat(col1("<Noteworthy string>\n  Format: "), format, col1("\n  Values: "),
      .tabr_print(x, col1), "\n", sep = "")
}

#' @export
summary.noteworthy <- function(object, ...){
  a <- attributes(object)
  col1 <- crayon::make_style("gray50")$bold
  cat(col1("<Noteworthy string>\n  Timesteps: "), a$steps, " (",
      a$n_note, " ", paste0("note", ifelse(a$n_note == 1, "", "s")), ", ",
      a$n_chord, " ", paste0("chord", ifelse(a$n_chord == 1, "", "s"), ")"),
      col1("\n  Octaves: "), a$octave,
      col1("\n  Accidentals: "), a$accidentals,
      col1("\n  Format: "), a$format, col1("\n  Values: "),
      .tabr_print(.uncollapse(as.character(object)), col1), sep = "")
}

.tabr_print <- function(x, col1){
  notes <- crayon::make_style("dodgerblue")$bold
  oct <- crayon::make_style("dodgerblue")
  other <- crayon::make_style("orange2")
  idx <- is_chord(x)
  if(any(idx)) x[idx] <- paste0("<", x[idx], ">")
  x <- paste(x, collapse = " ")
  x <- gsub("(\\d|[,']+)", oct("\\1"), x)
  x <- gsub("([a-grs_#]+)", notes("\\1"), x)
  x <- gsub("(~)", other("\\1"), x)
  x <- gsub("(<|>)", other("\\1"), x)
  x
}

#' Note, pitch and chord equivalence
#'
#' Helper functions to check the equivalence of two noteworthy strings, and
#' other related functions.
#'
#' Noteworthy strings may contain notes, pitches and chords. Noteworthy strings
#' are equal if they sound the same.
#' This means that if one string contains Eb (\code{e_}) and the other contains
#' D# (\code{d#}) then the two strings may be equal, but they are not identical.
#'
#' \code{pitch_is_equal} and \code{pitch_is_identical} perform these respective
#' tests of equivalence on both notes and chords.
#' These are the strictest functions in terms of equivalent sound because pitch
#' includes the octave number.
#'
#' \code{note_is_equal} and \code{note_is_identical} are similar but include a
#' default argument \code{ignore_octave = TRUE}, focusing only on the notes and
#' chords.
#' This allows an even more relaxed definition of equivalence. Setting this
#' argument to \code{FALSE} is the same as calling the \code{pitch_is_*}
#' variant.
#'
#' Chords can be checked the same as notes. Every timestep in the sequence is
#' checked pairwise between \code{note1} and \code{note2}.
#'
#' These functions will return \code{TRUE} or \code{FALSE} for every timestep
#' in a sequence.
#' If the two noteworthy strings do not contain the same number of notes at a
#' specific step, such as a single note compared to a chord, this yields a
#' \code{FALSE} value,
#' even in a case of an octave dyad with octave number ignored.
#' If the two sequences have unequal length \code{NA} is returned.
#' These are bare minimum requirements for equivalence. See examples.
#'
#' \code{octave_is_equal} and \code{octave_is_identical} allow much weaker
#' forms of equivalence in that they ignore notes completely.
#' These functions are only concerned with comparing the octave numbers spanned
#' by any pitches present at each timestep.
#' When checking for equality, \code{octave_is_equal} only looks at the octave
#' number associated with the first note at each step, e.g., only the root note
#' of a chord.
#' \code{octave_is_identical} compares all octaves spanned at a given timestep.
#'
#' It does not matter when comparing two chords that they may be comprised of a
#' different numbers of notes.
#' If the set of unique octaves spanned by one chord is identical to the set
#' spanned by the other, they are considered to have identical octave coverage.
#' For example, \code{a1b2c3} is identical to \code{d1e1f2g3}. To be equal, it
#' only matters that the two chords begin with \code{x1}, where \code{x} is any
#' note.
#' Alternatively, for \code{octave_is_identical} only, setting
#' \code{single_octave = TRUE} additionally requires that all notes from both
#' chords being compared at a given timestep share a single octave.
#'
#' @param notes1 character, noteworthy string, space-delimited or vector of
#' individual entries.
#' @param notes2 character, noteworthy string, space-delimited or vector of
#' individual entries.
#' @param ignore_octave logical, ignore octave position when considering
#' equivalence.
#' @param single_octave logical, for octave equality, require all notes share
#' the same octave. See details.
#'
#' @return logical
#' @export
#' @name note-equivalence
#'
#' @examples
#' x <- "b_2 ce_g"
#' y <- "b_ cd#g"
#' note_is_equal(x, y)
#' note_is_identical(x, y)
#'
#' x <- "b_2 ce_g"
#' y <- "b_2 cd#g"
#' pitch_is_equal(x, y)
#' pitch_is_identical(x, y)
#'
#' # same number of same notes, same order: unequal sequence length
#' x <- "b_2 ce_g b_"
#' y <- "b_2 ce_gb_"
#' note_is_equal(x, y)
#'
#' # same number of same notes, order, equal length: unequal number per timestep
#' x <- "b_2 ce_g b_"
#' y <- "b_2 ce_ gb_"
#' note_is_equal(x, y)
#'
#' x <- "a1 b_2 a1b2c3 a1b4 g1a1b1"
#' y <- "a_2 g#2 d1e1f2g3 a1b2b4 d1e1"
#' octave_is_equal(x, y)
#' octave_is_identical(x, y)
#' octave_is_identical(x, y, single_octave = TRUE)
note_is_equal <- function(notes1, notes2, ignore_octave = TRUE){
  x <- .check_comparison(notes1, notes2)
  if(any(is.na(x))) return(NA)
  if(ignore_octave){
    x[[1]] <- .pitch_to_note(x[[1]])
    x[[2]] <- .pitch_to_note(x[[2]])
  }
  sapply(seq_along(x[[1]]), function(i){
    length(.split_chord(x[[1]][i])) == length(.split_chord(x[[1]][i])) &
      flatten_sharp(x[[1]][i]) == flatten_sharp(x[[2]][i])
  })
}

#' @export
#' @rdname note-equivalence
note_is_identical <- function(notes1, notes2, ignore_octave = TRUE){
  x <- .check_comparison(notes1, notes2)
  if(any(is.na(x))) return(NA)
  if(ignore_octave){
    x[[1]] <- .pitch_to_note(x[[1]])
    x[[2]] <- .pitch_to_note(x[[2]])
  }
  y <- sapply(seq_along(x[[1]]), function(i){
    length(.split_chord(x[[1]][i])) == length(.split_chord(x[[1]][i]))
  })
  x[[1]] == x[[2]] & y
}

#' @export
#' @rdname note-equivalence
pitch_is_equal <- function(notes1, notes2){
  x <- .check_comparison(notes1, notes2)
  if(any(is.na(x))) return(NA)
  sapply(seq_along(x[[1]]), function(i){
    length(.split_chord(x[[1]][i])) == length(.split_chord(x[[1]][i])) &
      flatten_sharp(x[[1]][i]) == flatten_sharp(x[[2]][i])
  })
}

#' @export
#' @rdname note-equivalence
pitch_is_identical <- function(notes1, notes2){
  x <- .check_comparison(notes1, notes2)
  if(any(is.na(x))) return(NA)
  y <- sapply(seq_along(x[[1]]), function(i){
    length(.split_chord(x[[1]][i])) == length(.split_chord(x[[1]][i]))
  })
  x[[1]] == x[[2]] & y
}

#' @export
#' @rdname note-equivalence
octave_is_equal <- function(notes1, notes2){
  x <- .check_comparison(notes1, notes2)
  if(any(is.na(x))) return(NA)
  sapply(seq_along(x[[1]]), function(i){
    o1 <- as.integer(sapply(.split_chord(x[[1]][i]), .pitch_to_octave))[1]
    o2 <- as.integer(sapply(.split_chord(x[[2]][i]), .pitch_to_octave))[1]
    o1 == o2
  })
}

#' @export
#' @rdname note-equivalence
octave_is_identical <- function(notes1, notes2, single_octave = FALSE){
  x <- .check_comparison(notes1, notes2)
  if(any(is.na(x))) return(NA)
  sapply(seq_along(x[[1]]), function(i){
    o1 <- as.integer(sapply(.split_chord(x[[1]][i]), .pitch_to_octave))
    if(single_octave && length(o1) > 1 && !all(o1 == o1[1])) return(FALSE)
    o2 <- as.integer(sapply(.split_chord(x[[2]][i]), .pitch_to_octave))
    if(single_octave && length(o2) > 1 && !all(o2 == o2[1])) return(FALSE)
    if(any(!o1 %in% o2) | any(!o2 %in% o1)) return(FALSE)
    TRUE
  })
}

.check_comparison <- function(x1, x2){
  .check_noteworthy(x1)
  .check_noteworthy(x2)
  x1 <- .uncollapse(x1)
  x2 <- .uncollapse(x2)
  if(length(x1) != length(x2)) return(NA)
  list(x1, x2)
}
