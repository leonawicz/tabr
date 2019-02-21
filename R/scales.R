#' Scale helpers
#'
#' Helper functions for working with musical scales.
#'
#' For valid key signatures, see \code{\link{keys}}.
#'
#' @param key character, key signature.
#' @param root character, root note.
#' @param collapse logical, collapse result into a single string ready for phrase construction.
#' @param sharp logical, accidentals in arbitrary scale output should be sharp rather than flat.
#'
#' @return character
#' @export
#' @seealso \code{\link{keys}}, \code{\link{mode-helpers}}
#' @name scale-helpers
#'
#' @examples
#' scale_diatonic(key = "dm")
#' scale_minor(key = "d")
#' scale_major(key = "d")
#' scale_chromatic(root = "a")
scale_diatonic <- function(key = "c", collapse = FALSE){
  .keycheck(key)
  x <- .keydata[.keydata$key == key, ]
  base <- ifelse(x$major, "c d e f g a b", "a b c d e f g")
  x <- strsplit(transpose(base, x$c_am_rel_int, key, "strip"), " ")[[1]]
  if(collapse) x <- paste0(x, collapse = " ")
  note_set_key(x)
}

#' @export
#' @rdname scale-helpers
scale_major <- function(key = "c", collapse = FALSE){
  if(!key %in% .keydata$key[.keydata$major])
    stop("`key` does not indicate a valid major key.", call. = FALSE)
  scale_diatonic(key, collapse)
}

#' @export
#' @rdname scale-helpers
scale_minor <- function(key = "am", collapse = FALSE){
  if(!key %in% .keydata$key[!.keydata$major])
    stop("`key` does not indicate a valid minor key.", call. = FALSE)
  scale_diatonic(key, collapse)
}

#' @export
#' @rdname scale-helpers
scale_harmonic_minor <- function(key = "am", collapse = FALSE){
  x <- scale_minor(key)
  x[7] <- transpose(x[7], 1, key)
  if(collapse) x <- paste0(x, collapse = " ")
  x
}

# TODO # need to update this, scale_chords; add unit tests for these, for note_set_key, and for new chord helpers
#' #' @export
#' #' @rdname scale-helpers
#' scale_melodic_minor <- function(key = "am", collapse = FALSE){
#'   x <- scale_minor(key)
#'   x[7] <- transpose(x[7], 1, key)
#'   if(collapse) x <- paste0(x, collapse = " ")
#'   x
#' }

#' @export
#' @rdname scale-helpers
scale_chromatic <- function(root = "c", collapse = FALSE, sharp = TRUE){
  x <- ifelse(sharp, "c c# d d# e f f# g g# a a# b", "c d_ d e_ e f g_ g a_ a b_ b")
  y <- strsplit(x, " ")[[1]]
  if(!root %in% y) stop(paste("`root` is not one of:", x), call. = FALSE)
  idx <- match(root, y)
  if(idx != 1) y <- y[c(idx:length(y), 1:(idx - 1))]
  if(collapse) y <- paste0(y, collapse = " ")
  y
}

#' Diatonic chords
#'
#' Obtain an ordered sequence of the diatonic chords for a given scale.
#'
#' @param root character, root note or starting position of scale.
#' @param scale character, a valid named scale, referring to one of the existing \code{scale_*} functions.
#' @param type character, type of chord, either triad or seventh.
#' @param collapse logical, collapse result into a single string ready for phrase construction.
#' @param style character, passed to \code{transpose}.
#'
#' @return character
#' @export
#'
#' @examples
#' scale_chords("d", "major")
#' scale_chords("d", "minor")
#' scale_chords("d", "harmonic minor")
#' # scale_chords("d", "melodic minor") TODO
scale_chords <- function(root = "c", scale = "major", type = c("triad", "seventh"), collapse = FALSE, style = "default"){
  type <- match.arg(type)
  s <- paste0("scale_", scale)
  .check_scale_fun(s)
  s <- do.call(s, list(root = root))

  f <- switch(scale,
              "major" = list(xM, xm, xm, xM, xM, xm, xdim),
              "minor" = list(xm, xdim, xM, xm, xm, xM, xM),
              "harmonic minor" = list(xm, xdim, xaug, xm, xM, xM, xdim),
              "melodic minor" = list(xm, xm, xaug, xM, xM, xdim, xdim)
  )
  x <- sapply(seq_along(f), function(i) f[[i]](s[i]))
  r <- chord_order(x)[1] - 1
  if(r != 0){
    if(r <= 3){
      x[1:r] <- sapply(x[1:r], transpose, n = -12, style = style)
    } else {
      n <- length(x)
      x[(r + 1):n] <- sapply(x[(r + 1):n], transpose, n = 12, style = style)
    }
  }
  if(collapse) x <- paste0(x, collapse = " ")
  x
}

# nolint start

#' Scale degrees and mappings
#'
#' These functions assist with mapping between scale degrees, notes and chords.
#'
#' Obtain the scale degree of a note in a supported scale with \code{scale_degree}.
#' This function also works with chords inside note strings. It only considers the first note in each space-delimited entry.
#' \code{notes} may be a vector of single entries (non-delimited). Notes return \code{NA} if not explicitly in the scale.
#' This includes cases where the note is in the scale but is notated as sharp or flat when the \code{scale} and/or \code{key} provide the opposite.
#'
#' The inverse of \code{scale_degree} is \code{scale_note}, for obtaining the note associated with a scale degree.
#' This could be done simply by calling a \code{scale_*} function and indexing its output directly, but this wrapper is provided to complement \code{scale_degree}.
#' Additionally, it accepts the common Roman numeral input for the degree. This can be with the \code{roman} class or as a character string.
#' Degrees return \code{NA} if outside the scale degree range.
#'
#' \code{note_in_scale} performs a vectorized logical check if each note is in a given scale.
#' This function strictly accepts note strings.
#' To check if chords are diatonic to the scale, see \code{\link{chord_is_diatonic}}. To check generally if a \code{noteworthy} string is fully diatonic, see \code{\link{is_diatonic}}.
#'
#' @param notes character, a string of notes.
#' @param deg integer, roman class, or character roman, the scale degree.
#' @param key character, key signature (or root note) for scale, depending on the type of \code{scale}.
#' @param scale character, the suffix of a supported \code{scale_*} function.
#' @param naturalize logical, whether to naturalize any sharps or flats before obtaiuning the scale degree.
#' @param roman logical, return integer scale degrees as Roman numerals.
#' @param collapse logical, collapse result into a single string ready for phrase construction.
#' @param ... additional arguments passed to the scale function, e.g., \code{sharp = FALSE} for \code{scale_chromatic}.
#'
#' @return integer, or roman class if \code{roman = TRUE} for \code{scale_degree}. character for \code{scale_note}.
#' @export
#' @name scale-deg
#' @seealso \code{\link{scale-helpers}}
#'
#' @examples
#' scale_degree("c e g")
#' scale_degree("c e g", roman = TRUE)
#' scale_degree("c e g", key = "d")
#' scale_degree("c, e_3 g' f#ac#")
#' scale_degree("c, e_3 g' f#ac#", naturalize = TRUE)
#' scale_degree("c, e_3 g' f#ac#", scale = "chromatic")
#' scale_degree("c, e_3 g' f#ac#", scale = "chromatic", sharp = FALSE)
#'
#' scale_note(1:3)
#' scale_note(c(1, 3, 8), "d", collapse = TRUE)
#' all(sapply(list(4, "IV", as.roman(4)), scale_note) == "f")
scale_degree <- function(notes, key = "c", scale = "diatonic", naturalize = FALSE, roman = FALSE, ...){
  .check_noteworthy(notes)
  s <- paste0("scale_", scale)
  .check_scale_fun(s)
  x <- do.call(s, c(list(key), list(...)))
  notes <- .uncollapse(notes)
  notes <- substr(notes, 1, 2)
  f <- function(x) if(substr(x, 2, 2) %in% c("_", "#")) x else substr(x, 1, 1)
  notes <- sapply(notes, f)
  if(naturalize) notes <- naturalize(notes)
  x <- match(notes, x)
  if(roman) x <- utils::as.roman(x)
  x
}

# nolint end

#' @export
#' @rdname scale-deg
scale_note <- function(deg, key = "c", scale = "diatonic", collapse = FALSE, ...){
  s <- paste0("scale_", scale)
  .check_scale_fun(s)
  x <- do.call(s, c(list(key), list(...)))
  deg <- if(is.character(deg)) as.integer(utils::as.roman(strsplit(deg, " ")[[1]])) else as.integer(deg)
  if(any(deg < 1)) stop("`deg` should be >= 1.", call. = FALSE)
  x <- x[deg]
  if(collapse) x <- paste0(x, collapse = " ")
  x
}

#' @export
#' @rdname scale-deg
note_in_scale <- function(notes, key = "c", scale = "diatonic", ...){
  .check_note(notes)
  s <- paste0("scale_", scale)
  .check_scale_fun(s)
  x <- do.call(s, c(list(key), list(...)))
  .pitch_to_note(.uncollapse(notes)) %in% x
}

.check_scale_fun <- function(x){
  if(!exists(x, where = asNamespace("tabr"), mode = "function"))
    stop(paste0("`tabr::", x, "` is not an exported scale."), call. = FALSE)
}
