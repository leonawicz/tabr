#' Noteworthy string metadata
#'
#' Inspect basic metadata for noteworthy strings.
#'
#' @details
#' These functions inspect the basic metadata of noteworthy strings.
#' For functions that perform basic checks on strings, see
#' \code{\link{note-checks}}.
#'
#' The \code{n_*} functions give summary totals of the number of timesteps,
#' number of individual note (non-chord) timesteps, number of chord time
#' steps, and the number of distinct octaves present across timesteps.
#'
#' Functions pertaining to type or format of a noteworthy string provide
#' information on how a particular string is defined, e.g. \code{time_format}.
#' Note that the result pertains to true \code{noteworthy}-class objects. If
#' inspecting a standard character string, the result pertains to
#' post-conversion to the \code{noteworthy} class and does not necessarily
#' reflect what is found in \code{notes} verbatim. See examples.
#'
#' @section A note on generic functions:
#' \code{n_steps} and the three time format functions are generic since they
#' apply clearly to and are useful for not only noteworthy strings, but also
#' note info and music objects. If \code{x} is still a simple character string,
#' these functions attempt to guess which of the three it is. It is recommended
#' to set the class before using these functions.
#'
#' There are many package functions that operate on noteworthy strings that
#' could in concept also work on music objects, but the expectation is that
#' sound and time/info are disentangled for analysis.
#' The music class is convenient and relatively efficient data entry, e.g., for
#' transcription purposes, but it is not sensible to perform data analysis with
#' quantities like pitch and time tightly bound together in a single string.
#' This would only lead to repetitive deconstructions and reconstructions of
#' music class objects.
#'
#' The music class is intended to be a transient class such as during data
#' import, data entry, or data export.
#' Most functions that operate on noteworthy strings or note info
#' strings strictly apply to one or the other. Generic functions are reserved
#' for only the most fundamental and generally applicable metadata retrieval
#' and format coercion.
#'
#' @param notes character, a noteworthy string, space-delimited or vector of
#' individual entries.
#' @param x for generic functions: notes, info or music string.
#'
#' @return varies by function
#' @export
#' @name note-metadata
#' @seealso \code{\link{tabr-methods}}, \code{\link{note-checks}},
#' \code{\link{note-summaries}}, \code{\link{note-coerce}},
#' \code{\link{valid-notes}}
#'
#' @examples
#' x <- "e_2 a_, c#f#a#"
#' n_steps(x)
#' n_notes(x)
#' n_chords(x)
#' n_octaves(x)
#'
#' # Type is mixed in \code{x} but is inferred under default conversion rules.
#' # These check \code{x} once validated and coerced to 'noteworthy' class.
#' octave_type(x)
#' accidental_type(x)
#' # The default is tick octaves and flats
#' as_noteworthy(x)
#'
#' time_format(x)
#' is_space_time(x)
#' is_vector_time(x)
n_steps <- function(x){
  UseMethod("n_steps", x)
}

#' @export
n_steps.noteworthy <- function(x){
  attr(as_noteworthy(x), "steps")
}

#' @export
n_steps.noteinfo <- function(x){
  attr(as_noteinfo(x), "steps")
}

#' @export
n_steps.music <- function(x){
  attr(as_music(x), "steps")
}

#' @export
n_steps.numeric <- function(x){
  n_steps.noteinfo(x)
}

#' @export
n_steps.character <- function(x){
  switch(
    .guess_string_type(x),
    "noteworthy" = n_steps.noteworthy(x),
    "noteinfo" = n_steps.noteinfo(x),
    "music" = n_steps.music(x)
  )
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
octave_type <- function(notes){
  attr(as_noteworthy(notes), "octave")
}

#' @export
#' @rdname note-metadata
accidental_type <- function(x){
  y <- .guess_string_type(x, try_info = FALSE)
  switch(y,
    "noteworthy" = attr(as_noteworthy(x), "accidentals"),
    "music" = attr(as_music(x), "accidentals")
  )
}

#' @export
#' @rdname note-metadata
time_format <- function(x){
  UseMethod("time_format", x)
}

#' @export
time_format.noteworthy <- function(x){
  attr(as_noteworthy(x), "format")
}

#' @export
time_format.noteinfo <- function(x){
  attr(as_noteinfo(x), "format")
}

#' @export
time_format.music <- function(x){
  attr(as_music(x), "format")
}

time_format.numeric <- function(x){
  time_format.noteinfo(x)
}

#' @export
time_format.character <- function(x){
  switch(
    .guess_string_type(x),
    "noteworthy" = time_format.noteworthy(x),
    "noteinfo" = time_format.noteinfo(x),
    "music" = time_format.music(x)
  )
}

#' @export
#' @rdname note-metadata
is_space_time <- function(x){
  UseMethod("is_space_time", x)
}

#' @export
is_space_time.noteworthy <- function(x){
  time_format.noteworthy(x) == "space-delimited time"
}

#' @export
is_space_time.noteinfo<- function(x){
  time_format.noteinfo(x) == "space-delimited time"
}

#' @export
is_space_time.music <- function(x){
  time_format.music(x) == "space-delimited time"
}

#' @export
is_space_time.numeric <- function(x){
  is_space_time.noteinfo(x)
}

#' @export
is_space_time.character <- function(x){
  switch(
    .guess_string_type(x),
    "noteworthy" = is_space_time.noteworthy(x),
    "noteinfo" = is_space_time.noteinfo(x),
    "music" = is_space_time.music(x)
  )
}

#' @export
#' @rdname note-metadata
is_vector_time <- function(x){
  UseMethod("is_vector_time", x)
}

#' @export
is_vector_time.noteworthy <- function(x){
  time_format.noteworthy(x) == "vectorized time"
}

#' @export
is_vector_time.noteinfo <- function(x){
  time_format.noteinfo(x) == "vectorized time"
}

#' @export
is_vector_time.music <- function(x){
  time_format.music(x) == "vectorized time"
}

#' @export
is_vector_time.numeric <- function(x){
  is_vector_time.noteinfo(x)
}

#' @export
is_vector_time.character <- function(x){
  switch(
    .guess_string_type(x),
    "noteworthy" = is_vector_time.noteworthy(x),
    "noteinfo" = is_vector_time.noteinfo(x),
    "music" = is_vector_time.music(x)
  )
}

#' Noteworthy string summaries
#'
#' Basic summary functions for noteworthy strings.
#'
#' These functions provide basic summaries of noteworthy strings.
#'
#' Returned object depends on the nature of the function. It can be integers,
#' logical, character. Results can be a vector of equal length of a single
#' value summary.
#'
#' Use the \code{tally_*} and \code{distinct_*} functions specifically for
#' summaries of unique elements.
#'
#' \code{distinct_notes} and \code{distinct_pitches} filter a noteworthy string
#' to its unique elements, respectively. These functions return another
#' noteworthy string.
#'
#' \code{*_span} functions are just the size of a range, e.g.,
#' \code{semitone_range} and \code{semitone_span}.
#'
#' @param notes character, a noteworthy string, space-delimited or vector of
#' individual entries.
#' @param rests logical, include rests \code{r} and silent rests \code{s} in
#' tally.
#'
#' @return varies by function
#' @export
#' @name note-summaries
#' @seealso \code{\link{note-checks}}, \code{\link{note-metadata}},
#' \code{\link{note-coerce}}, \code{\link{valid-notes}}
#'
#' @examples
#' x <- "r s e_2 a_, c#f#a#"
#' tally_notes(x)
#' tally_pitches(x)
#' octaves(x)
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
tally_notes <- function(notes, rests = FALSE){
  .check_noteworthy(notes)
  x <- .pitch_to_note(.split_chords(.uncollapse(notes)))
  x <- x[x != " "]
  x <- as.data.frame(table(x), stringsAsFactors = FALSE) %>%
    tibble::as_tibble() %>%
    stats::setNames(c("note", "n"))
  if(!rests) x <- x[!x$note %in% c("r", "s"), ]
  x[.pitch_order(x$note), ]
}

#' @export
#' @rdname note-summaries
tally_pitches <- function(notes, rests = FALSE){
  .check_noteworthy(notes)
  x <- .split_chords(.uncollapse(notes))
  x <- x[x != " "]
  x <- as.data.frame(table(x), stringsAsFactors = FALSE) %>%
    tibble::as_tibble() %>%
    stats::setNames(c("pitch", "n"))
  if(!rests) x <- x[!x$pitch %in% c("r", "s"), ]
  x$pitch <- gsub("~", "", x$pitch)
  x[.pitch_order(x$pitch), ]
}

#' @export
#' @rdname note-summaries
octaves <- function(notes){
  .check_noteworthy(notes)
  x <- .uncollapse(notes)
  idx <- sapply(gregexpr("[a-g]", x), length) > 1
  y <- rep(3L, length(x))
  if(any(!idx)) y[!idx] <- .pitch_to_octave(x[!idx])
  y[x %in% c("r", "s")] <- NA_integer_
  x <- as.list(x)
  if(any(idx))
    y[idx] <- purrr::map(x[idx], ~.pitch_to_octave(.split_chords(.x)))
  y
}

#' @export
#' @rdname note-summaries
tally_octaves <- function(notes){
  x <- unlist(octaves(notes))
  as.data.frame(table(x), stringsAsFactors = FALSE) %>%
    tibble::as_tibble() %>%
    stats::setNames(c("octave", "n")) %>%
    dplyr::mutate(octave = as.integer(.data[["octave"]]))
}

#' @export
#' @rdname note-summaries
distinct_notes <- function(notes, rests = FALSE){
  x <- tally_notes(notes, rests)$note
  if(time_format(notes) == "space-delimited time") x <- paste(x, collapse = " ")
  .asnw(x)
}

#' @export
#' @rdname note-summaries
distinct_pitches <- function(notes, rests = FALSE){
  x <- tally_pitches(notes, rests)$pitch
  if(time_format(notes) == "space-delimited time") x <- paste(x, collapse = " ")
  .asnw(x)
}

#' @export
#' @rdname note-summaries
distinct_octaves <- function(notes){
  tally_octaves(notes)$octave
}

#' @export
#' @rdname note-summaries
pitch_range <- function(notes){
  x <- .uncollapse(distinct_pitches(notes))
  if(length(x) == 1) c(x, x) else c(x[1], utils::tail(x, 1))
}

#' @export
#' @rdname note-summaries
semitone_range <- function(notes){
  pitch_semitones(pitch_range(notes))
}

#' @export
#' @rdname note-summaries
semitone_span <- function(notes){
  diff(semitone_range(notes))
}

#' @export
#' @rdname note-summaries
octave_range <- function(notes){
  as.integer(range(distinct_octaves(notes)))
}

#' @export
#' @rdname note-summaries
octave_span <- function(notes){
  diff(octave_range(notes))
}

#' Basic noteworthy string checks
#'
#' The simplest functions for inspecting noteworthy strings to see if their
#' notes have certain properties.
#'
#' Note that these functions are the weakest in terms of checking
#' noteworthiness. They are simple regular expression-based wrappers. They are
#' often used internally by more complex functions without wasting computational
#' overhead on performing input validity checks, but they are exported from the
#' package for user convenience. Their results will only make sense on strings
#' that you define in accordance with noteworthy string rules.
#'
#' The \code{note_is_*} functions return a logical vector with length equal to
#' the number of timesteps in \code{notes}.
#' The \code{note_has_*} functions summarize these to a single logical value.
#'
#' @param notes character, a noteworthy string.
#'
#' @return logical
#' @export
#' @name note-checks
#' @seealso \code{\link{note-metadata}}, \code{\link{note-summaries}},
#' \code{\link{note-coerce}}, \code{\link{valid-notes}}
#'
#' @examples
#' x <- "a_2 a a#'"
#' note_has_accidental(x)
#' note_has_natural(x)
#' note_has_flat(x)
#' note_has_sharp(x)
#' note_is_accidental(x)
#' note_is_natural(x)
#' note_is_flat(x)
#' note_is_sharp(x)
#' note_has_tick(x)
#' note_has_integer(x)
#' note_is_tick(x)
#' note_is_integer(x)
note_is_accidental <- function(notes){
  grepl("_|#", .uncollapse(notes))
}

#' @export
#' @rdname note-checks
note_is_natural <- function(notes){
  !note_is_accidental(notes)
}

#' @export
#' @rdname note-checks
note_is_flat <- function(notes){
  grepl("_", .uncollapse(notes))
}

#' @export
#' @rdname note-checks
note_is_sharp <- function(notes){
  grepl("#", .uncollapse(notes))
}

#' @export
#' @rdname note-checks
note_has_accidental <- function(notes){
  any(note_is_accidental(notes))
}

#' @export
#' @rdname note-checks
note_has_natural <- function(notes){
  any(note_is_natural(notes))
}

#' @export
#' @rdname note-checks
note_has_flat <- function(notes){
  any(note_is_flat(notes))
}

#' @export
#' @rdname note-checks
note_has_sharp <- function(notes){
  any(note_is_sharp(notes))
}

#' @export
#' @rdname note-metadata
note_is_tick <- function(notes){
  grepl("[,']", .uncollapse(notes))
}

#' @export
#' @rdname note-metadata
note_is_integer <- function(notes){
  grepl("\\d", .uncollapse(notes))
}

#' @export
#' @rdname note-metadata
note_has_tick <- function(notes){
  any(note_is_tick(.uncollapse(notes)))
}

#' @export
#' @rdname note-metadata
note_has_integer <- function(notes){
  any(note_is_integer(.uncollapse(notes)))
}

#' Basic noteworthy strings formatting and coercion helpers
#'
#' Helper functions for setting formatting attributes of
#' noteworthy strings including representation of timesteps, octaves and
#' accidentals.
#'
#' @details
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
#' @section A note on generic functions:
#' \code{as_space_time} and \code{as_vector_time} are generic since they
#' apply clearly to and are useful for not only noteworthy strings, but also
#' note info and music objects. If \code{x} is still a simple character string,
#' these functions attempt to guess which of the three it is. It is recommended
#' to set the class before using these functions.
#'
#' There are many package functions that operate on noteworthy strings that
#' could in concept work on music objects, but the expectation is that sound
#' and time/info are disentangled.
#' The music class is convenient for data entry, e.g., for transcription
#' purposes, but it is not sensible to perform data analysis with quantities
#' like pitch and time tightly bound together. This would only lead to
#' repetitive deconstructions and reconstructions of music class objects. Most
#' functions that operate on noteworthy strings or note info strings strictly
#' apply to one or the other. Generic functions are reserved for only the most
#' fundamental and generally applicable metadata retrieval and format coercion.
#'
#' @param notes character, a noteworthy string, space-delimited or vector of
#' individual entries.
#' @param type character, type of note to naturalize.
#' @param ignore_octave logical, strip any octave notation that may be present,
#' returning only the basic notes without explicit pitch.
#' @param key character, key signature to coerce any accidentals to the
#' appropriate form for the key. May also specify \code{"sharp"} or
#' \code{"flat"}.
#' @param x for generic functions: notes, info or music string.
#'
#' @return character
#' @export
#' @name note-coerce
#' @seealso \code{\link{note-checks}}, \code{\link{note-metadata}},
#' \code{\link{note-summaries}}, \code{\link{valid-notes}}
#'
#' @examples
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
#' @export
#' @rdname note-coerce
naturalize <- function(notes, type = c("both", "flat", "sharp")){
  .check_noteworthy(notes)
  type <- match.arg(type)
  pat <- switch(type, both = "_|#", flat = "_", sharp = "#")
  x <- gsub(pat, "", notes)
  .asnw(x)
}

#' @export
#' @rdname note-coerce
sharpen_flat <- function(notes){
  .check_noteworthy(notes)
  x <- .uncollapse(notes) %>% .flat_to_sharp()
  if(length(notes) == 1) x <- paste(x, collapse = " ")
  .asnw(x)
}

#' @export
#' @rdname note-coerce
flatten_sharp <- function(notes){
  .check_noteworthy(notes)
  x <- .uncollapse(notes) %>% .sharp_to_flat()
  if(length(notes) == 1) x <- paste(x, collapse = " ")
  .asnw(x)
}

#' @export
#' @rdname note-coerce
note_set_key <- function(notes, key = "c"){
  if(key == "flat") return(as_noteworthy(.sharp_to_flat(notes)))
  if(key == "sharp") return(as_noteworthy(.flat_to_sharp(notes)))
  .keycheck(key)
  if(key_is_natural(key)) return(as_noteworthy(notes))
  Recall(notes, .keydata$sf[.keydata$key == key])
}

#' @export
#' @rdname note-coerce
as_tick_octaves <- function(notes){
  as_noteworthy(notes, octaves = "tick")
}

#' @export
#' @rdname note-coerce
as_integer_octaves <- function(notes){
  as_noteworthy(notes, octaves = "integer")
}

#' @export
#' @rdname note-coerce
as_space_time <- function(x){
  UseMethod("as_space_time", x)
}

#' @export
as_space_time.noteworthy <- function(x){
  .asnw(x, format = "space")
}

#' @export
as_space_time.noteinfo <- function(x){
  .asni(x, format = "space")
}

#' @export
as_space_time.music <- function(x){
  x <- music_split(x)
  .asmusic(x$notes, x$info, tsig = x$tsig, format = "space")
}

#' @export
as_space_time.numeric <- function(x){
  as_space_time.noteinfo(x)
}

#' @export
as_space_time.character <- function(x){
  switch(
    .guess_string_type(x),
    "noteworthy" = as_space_time.noteworthy(x),
    "noteinfo" = as_space_time.noteinfo(x),
    "music" = as_space_time.music(x)
  )
}

#' @export
#' @rdname note-coerce
as_vector_time <- function(x){
  UseMethod("as_vector_time", x)
}

#' @export
as_vector_time.noteworthy <- function(x){
  .asnw(x, format = "vector")
}

#' @export
as_vector_time.noteinfo <- function(x){
  .asni(x, format = "vector")
}

#' @export
as_vector_time.music <- function(x){
  x <- music_split(x)
  .asmusic(x$notes, x$info, tsig = x$tsig, format = "vector")
}

#' @export
as_vector_time.numeric <- function(x){
  as_vector_time.noteinfo(x)
}

#' @export
as_vector_time.character <- function(x){
  switch(
    .guess_string_type(x),
    "noteworthy" = as_vector_time.noteworthy(x),
    "noteinfo" = as_vector_time.noteinfo(x),
    "music" = as_vector_time.music(x)
  )
}

#' @export
#' @rdname note-coerce
pretty_notes <- function(notes, ignore_octave = TRUE){
  .check_noteworthy(notes)
  if(ignore_octave) notes <- .pitch_to_note(notes)
  gsub("~", "", gsub("_", "b", toupper(notes)))
}

#' Slice, sort, rotate, shift and arpeggiate notes
#'
#' Helper functions for indexing and moving notes within noteworthy strings.
#'
#' \code{note_slice} subsets the timesteps of a noteworthy string by integer
#' index or logical vector of length equal to the number of timesteps.
#'
#' \code{note_sort} sorts the timesteps of a noteworthy string by pitch. When a
#' tie exists by root note, the next note in chords are compared, if they exist.
#' For example, \code{a,} sorts lower than \code{a,ce}.
#'
#' \code{note_rotate} simply rotates anything space-delimited or vectorized in
#' place. It allows chords. Octave numbering is ignored if present.
#'
#' For \code{note_shift} the entire sequence is shifted up or down in pitch, as
#' if inverting a broken chord.
#' If \code{notes} contains chords, they are broken into successive notes. Then
#' all notes are ordered by pitch. Finally shifting occurs.
#'
#' Instead of a moving window, \code{note_arpeggiate} grows its sequence from
#' the original set of timesteps by repeating the entire sequence \code{n}
#' times (\code{n} must be positive). Each repeated sequence contributing to
#' the arpeggio is offset by \code{step} semitones from the original.
#' \code{step} can be negative. It defaults to 12, increasing all \code{notes}
#' by one octave.
#'
#' @param notes character, a noteworthy string, space-delimited or vector of
#' individual entries.
#' @param decreasing logical, short in decreasing order.
#' @param n integer, number of rotations or extensions of note sequence. See
#' details.
#' @param step integer, number of semitone steps from the first (or last) note
#' in \code{notes} at which to begin repeating the shifted \code{notes}
#' sequence as an arpeggio. See examples.
#' arpeggio.
#' @param ... For \code{note_slice}, an integer or logical vector.
#'
#' @return character
#' @export
#'
#' @examples
#' x <- "bd'f#' a c'e'g' b ba c'g' gd'g'd''"
#' note_sort(x)
#' note_sort(x, decreasing = TRUE)
#'
#' x <- "e_2 a_, c#f#a#"
#' note_slice(x, 2:3)
#' note_slice(x, c(FALSE, TRUE, TRUE))
#'
#' note_rotate(x, 1)
#'
#' note_shift("c e g", 1)
#' note_shift("c e g", -4)
#'
#' note_arpeggiate("c e g ceg", 3)
#' note_arpeggiate("c e g", 3, step = -12)
#' note_arpeggiate("g e c", 3, step = -12)
#' note_arpeggiate("c e_ g_ a", 3, step = 3)
#' note_arpeggiate("c a g_ e_", 3, step = -3)
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
note_sort <- function(notes, decreasing = FALSE){
  .check_noteworthy(notes)
  x <- .uncollapse(notes)
  s <- lapply(chord_semitones(x), sort)
  n <- max(sapply(s, length))
  s <- purrr::map(s, ~{
    x <- rep(NA_integer_, n)
    x[seq_along(.x)] <- .x
    x[is.na(x)] <- utils::tail(.x, 1)
    x
  })
  d <- as.data.frame(t(as.data.frame(s)))
  d <- tibble::as_tibble(d) %>% dplyr::mutate(x = x)
  x <- dplyr::arrange_at(d, seq_len(ncol(d))[-c(n + 1)])$x
  if(decreasing) x <- rev(x)
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
  x <- x[c((n + 1):length(x), 1:n)]
  if(length(notes) == 1) x <- paste0(x, collapse = " ")
  .asnw(x)
}

#' @export
#' @rdname note_slice
note_shift <- function(notes, n = 0){
  .check_noteworthy(notes)
  z <- .infer_types(notes)
  x <- .uncollapse(notes) %>% .split_chords() %>%
    .pitch_semitones(z$o, z$a)
  x <- sort(x[!is.na(x)])
  len <- length(x)
  if(n == 0 & len == 1){
    x <- notes
  } else if(n == 0){
    x <- semitone_pitch(x)
  } else if(n > 0){
    if(len == 1){
      x <- semitone_pitch(x + 12 * n)
    } else {
      for(i in seq_len(n)){
        v <- x[1] + 12
        while(v <= x[len]) v <- v + 12
        x <- c(x[-1], v)
      }
      x <- semitone_pitch(x)
    }
  } else if(n < 0){
    if(len == 1){
      x <- semitone_pitch(x + 12 * n)
    } else {
      for(i in seq_len(-n)){
        v <- x[len] - 12
        while(v >= x[1]) v <- v - 12
        x <- c(v, x[-len])
      }
      x <- semitone_pitch(x)
    }
  }
  .asnw(x, z$o, z$a, if(length(notes) == 1) "space" else "vector")
}

#' @export
#' @rdname note_slice
note_arpeggiate <- function(notes, n = 0, step = 12){
  if(n < 0) stop("`n` cannot be negative.", call. = FALSE)
  .check_noteworthy(notes)
  format <- if(length(notes) == 1) "space" else "vector"
  z <- .infer_types(notes)
  x <- .uncollapse(notes) %>% .split_chords() %>%
    .pitch_semitones()
  if(n == 0){
    x <- semitone_pitch(x)
    x <- x[!is.na(x)]
  } else {
    y <- vector("list", n + 1)
    y[[1]] <- x
    for(i in 2:length(y)) y[[i]] <- y[[i - 1]] + step
    x <- purrr::map(y, ~{
      x <- semitone_pitch(.x)
      x[is.na(x)] <- " "
      paste(x, collapse = "")
    }) %>% unlist() %>% paste(collapse = " ")
  }
  .asnw(x, z$o, z$a, format)
}

#' Check note and chord validity
#'
#' Check whether a string is comprised exclusively of valid note and/or chord
#' syntax.
#' \code{is_note} and \code{is_chord} are vectorized and their positive results
#' are mutually exclusive.
#' \code{noteworthy} is also vectorized and performs both checks, but it
#' returns a scalar logical result indicating whether the entire set contains
#' exclusively valid entries.
#'
#' \code{as_noteworthy} can be used to coerce to the \code{noteworthy} class.
#' Coercion will fail if the string is not noteworthy.
#' While many functions will work on simple character strings and, if their
#' syntax is valid, coerce them to the 'noteworthy' class, it is recommended to
#' use this class. Not all functions are so aggressive, and several generic
#' methods are implemented for the class. It also offers its own \code{print}
#' and \code{summary} methods for noteworthy strings.
#' An added benefit to using \code{as_noteworthy} is to conform all
#' notes in a noteworthy string to specific formatting for accidentals and
#' octave numbering.
#' Functions that output a noteworthy string attach the \code{noteworthy} class.
#'
#' When \code{octaves}, \code{accidentals}, and \code{format} are \code{NULL},
#' formatting is inferred from the noteworthy string input. When mixed formats
#' are present, tick format is the default for octave numbering and flats are
#' the default for accidentals.
#'
#' @param x character, a noteworthy string.
#' @param octaves \code{NULL} or character, \code{"tick"} or \code{"integer"}
#' octave numbering in result.
#' @param accidentals \code{NULL} or character, represent accidentals,
#' \code{"flat"} or \code{"sharp"}.
#' @param format \code{NULL} or character, the timestep delimiter format,
#' \code{"space"} or \code{"vector"}.
#' @param na.rm remove \code{NA}s.
#'
#' @return depends on the function
#' @export
#' @name valid-notes
#' @seealso \code{\link{note-checks}}, \code{\link{note-metadata}},
#' \code{\link{note-summaries}}, \code{\link{note-coerce}}
#'
#' @examples
#' x <- "a# b_ c, d'' e3 g_4 A m c2e_2g2 cegh" # includes invalid syntax
#' data.frame(
#'   x = strsplit(x, " ")[[1]],
#'   note = is_note(x),
#'   chord = is_chord(x),
#'   either = noteworthy(x))
#'
#' is_diatonic("ace ac#e d e_", "c")
#'
#' x <- "a# b_ c,~ c, d'' e3 g_4 c2e_2g2"
#' noteworthy(x) # is it noteworthy; a validity check for any string
#' x <- as_noteworthy(x) # coerce to 'noteworthy' class, conform formatting
#' is_noteworthy(x) # check for 'noteworthy' class
#' x
#'
#' summary(x)
#'
#' x <- as_noteworthy(x, format = "vector", octaves = "integer",
#'                    accidentals = "flat")
#' x
#'
#' summary(x)
is_note <- function(x, na.rm = FALSE){
  if(na.rm){
    x <- x[!is.na(x)]
    if(!is.character(x)) x <- as.character(x)
  }
  x <- .uncollapse(x)
  y1 <- grepl("[a-grs]", x) & !grepl("[h-qt-zA-Z]", x)
  y2 <- gsub("\\d|,|'|_|#|~|\\*", "", x)
  y1 & nchar(y2) == 1 & y2 == substr(x, 1, 1) & !grepl("(r|s)\\d", x)
}

#' @export
#' @rdname valid-notes
is_chord <- function(x, na.rm = FALSE){
  if(na.rm){
    x <- x[!is.na(x)]
    if(!is.character(x)) x <- as.character(x)
  }
  x <- .uncollapse(x)
  len <- sapply(gregexpr("[a-g]", x), length)
  idx <- len > 1
  y <- rep(FALSE, length(x))
  if(any(idx)){
    y[idx] <- sapply(x[idx], function(x) all(is_note(.split_chords(x))),
                     USE.NAMES = FALSE)
  }
  y
}

#' @export
#' @rdname valid-notes
noteworthy <- function(x, na.rm = FALSE){
  if(is_noteworthy(x)) return(TRUE)
  all(is_note(x, na.rm) | is_chord(x, na.rm))
}

.asnw <- function(x, octaves = NULL, accidentals = NULL, format = NULL){
  if(is.null(octaves)) octaves <- .infer_octave_type(x)
  if(is.null(accidentals)) accidentals <- .infer_accidentals(x)
  if(is.null(format)) format <- .infer_time_format(x)
  format <- switch(format, space = "space-delimited time",
                   vector = "vectorized time")
  x <- .pitch_conform(.uncollapse(x), octaves, accidentals)
  steps <- length(x)
  nnote <- as.integer(sum(is_note(x)))
  nchord <- as.integer(sum(is_chord(x)))
  has_acc <- switch(accidentals,
                    "flat" = note_has_flat, "sharp" = note_has_sharp)
  a <- if(has_acc(x)) accidentals else "flat"
  tick <- any(grepl(",|'", x))
  int <- any(grepl("\\d", x))
  has_oct <- switch(octaves,
                    "tick" = note_has_tick, "integer" = note_has_integer)
  o <- if(has_oct(x)) octaves else "tick"
  if(format == "space-delimited time") x <- paste(x, collapse = " ")
  attributes(x) <- list(steps = steps, n_note = nnote, n_chord = nchord,
                        octave = o, accidentals = a, format = format)
  class(x) <- unique(c("noteworthy", class(x)))
  x
}

#' @export
#' @rdname valid-notes
as_noteworthy <- function(x, octaves = NULL, accidentals = NULL, format = NULL){
  null_args <- all(sapply(list(format, octaves, accidentals), is.null))
  if(inherits(x, "noteworthy") & null_args) return(x)
  .check_noteworthy(x)
  .check_format_arg(format)
  .check_octaves_arg(octaves)
  .check_accidentals_arg(accidentals)
  .asnw(x, octaves, accidentals, format)
}

.check_format_arg <- function(x){
  if(!is.null(x)){
    if(!x %in% c("space", "vector")){
      stop("`format` must be 'space' or 'vector' if not NULL.", call. = FALSE)
    }
  }
}

.check_octaves_arg <- function(x){
  if(!is.null(x)){
    if(!x %in% c("tick", "integer")){
      stop("`octaves` must be 'tick' or 'integer' if not NULL.", call. = FALSE)
    }
  }
}

.check_accidentals_arg <- function(x){
  if(!is.null(x)){
    if(!x %in% c("flat", "sharp"))
      stop("`accidentals` must be 'flat' or 'sharp' if not NULL.",
           call. = FALSE)
  }
}

#' @export
#' @rdname valid-notes
is_noteworthy <- function(x){
  inherits(x, "noteworthy")
}

.check_note <- function(x, na.rm = FALSE){
  if(any(!is_note(x, na.rm)))
    stop("Invalid note found.", call. = FALSE)
}

.check_chord <- function(x, na.rm = FALSE){
  if(any(!is_chord(x, na.rm)))
    stop("Invalid chord found.", call. = FALSE)
}

.check_noteworthy <- function(x, na.rm = FALSE){
  if(!noteworthy(x, na.rm))
    stop("Invalid notes or chords found.", call. = FALSE)
}

#' @export
print.noteworthy <- function(x, ...){
  a <- attributes(x)
  col1 <- crayon::make_style("gray50")$bold
  x <- .uncollapse(x)
  cat(col1("<Noteworthy string>\n  Format: "), a$format, col1("\n  Values: "),
      .tabr_print(x), "\n", sep = "")
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
      .tabr_print(.uncollapse(object)), "\n", sep = "")
}

.tabr_print <- function(x){
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
  .check_noteworthy(notes1)
  .check_noteworthy(notes2)
  x <- .pitch_to_note(.pitch_conform(.uncollapse(notes1)))
  y <- .pitch_to_note(.pitch_conform(.uncollapse(notes2)))
  if(length(x) != length(y)) return(NA)
  x == y
}

#' @export
#' @rdname note-equivalence
note_is_identical <- function(notes1, notes2, ignore_octave = TRUE){
  .check_noteworthy(notes1)
  .check_noteworthy(notes2)
  x <- .pitch_to_note(.uncollapse(notes1))
  y <- .pitch_to_note(.uncollapse(notes2))
  if(length(x) != length(y)) return(NA)
  x == y
}

#' @export
#' @rdname note-equivalence
pitch_is_equal <- function(notes1, notes2){
  .check_noteworthy(notes1)
  .check_noteworthy(notes2)
  x <- .pitch_conform(.uncollapse(notes1))
  y <- .pitch_conform(.uncollapse(notes2))
  if(length(x) != length(y)) return(NA)
  x == y
}

#' @export
#' @rdname note-equivalence
pitch_is_identical <- function(notes1, notes2){
  .check_noteworthy(notes1)
  .check_noteworthy(notes2)
  x <- .uncollapse(notes1)
  y <- .uncollapse(notes2)
  if(length(x) != length(y)) return(NA)
  x == y
}

#' @export
#' @rdname note-equivalence
octave_is_equal <- function(notes1, notes2){
  x <- octaves(notes1)
  y <- octaves(notes2)
  if(length(x) != length(y)) return(NA)
  purrr::map2_lgl(x, y, ~{
    length(dplyr::intersect(.x, .y)) > 0
  })
}

#' @export
#' @rdname note-equivalence
octave_is_identical <- function(notes1, notes2, single_octave = FALSE){
  x <- octaves(notes1)
  y <- octaves(notes2)
  if(length(x) != length(y)) return(NA)
  purrr::map2_lgl(x, y, ~{
    if(single_octave){
      if(length(unique(c(.x, .y))) == 1) return(TRUE) else return(FALSE)
    } else {
      all(.x %in% .y) & all(.y %in% .x)
    }
  })
}
