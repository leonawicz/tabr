#' Scale helpers
#'
#' Helper functions for working with musical scales.
#'
#' For valid key signatures, see \code{\link{keys}}.
#'
#' @param key character, key signature.
#' @param root character, root note.
#' @param collapse logical, collapse result into a single string ready for
#' phrase construction.
#' @param sharp logical, accidentals in arbitrary scale output should be sharp
#' rather than flat.
#' @param descending logical, return the descending scale, available as a
#' built-in argument for the melodic minor scale, which is different in each
#' direction.
#' @param ignore_octave logical, strip octave numbering from scales not rooted
#' on C.
#'
#' @return character
#' @export
#' @seealso \code{\link{keys}}, \code{\link{mode-helpers}}
#' @name scale-helpers
#'
#' @examples
#' scale_diatonic(key = "dm")
#' scale_minor(key = "dm")
#' scale_major(key = "d")
#'
#' scale_chromatic(root = "a")
#'
#' scale_harmonic_minor("am")
#' scale_hungarian_minor("am")
#'
#' identical(scale_melodic_minor("am"), scale_jazz_minor("am"))
#' rev(scale_melodic_minor("am", descending = TRUE))
#' scale_jazz_minor("am")
scale_diatonic <- function(key = "c", collapse = FALSE, ignore_octave = FALSE){
  .keycheck(key)
  x <- .keydata[.keydata$key == key, ]
  base <- if(x$major) letters[c(3:7, 1:2)] else letters[1:7]
  x <- transpose(base, x$c_am_rel_int, key = key) %>% .pitch_to_note()
  if(!ignore_octave) x <- .scale_set_octave(x, "tick")
  if(collapse) x <- paste0(x, collapse = " ")
  note_set_key(x, key)
}

#' @export
#' @rdname scale-helpers
scale_major <- function(key = "c", collapse = FALSE, ignore_octave = FALSE){
  if(!key %in% .keydata$key[.keydata$major])
    stop("`key` does not indicate a valid major key.", call. = FALSE)
  scale_diatonic(key, collapse, ignore_octave)
}

#' @export
#' @rdname scale-helpers
scale_minor <- function(key = "am", collapse = FALSE,
                        ignore_octave = FALSE){
  if(!key %in% .keydata$key[!.keydata$major])
    stop("`key` does not indicate a valid minor key.", call. = FALSE)
  scale_diatonic(key, collapse, ignore_octave)
}

#' @export
#' @rdname scale-helpers
scale_harmonic_minor <- function(key = "am", collapse = FALSE,
                                 ignore_octave = FALSE){
  x <- scale_minor(key, ignore_octave = ignore_octave)
  tkey <- if(key_is_natural(key)) "g" else key
  x[7] <- transpose(x[7], 1, key = tkey)
  if(collapse) x <- paste0(x, collapse = " ")
  a <- if(key_is_flat(tkey)) "flat" else "sharp"
  .asnw(x, accidentals = a)
}

#' @export
#' @rdname scale-helpers
scale_hungarian_minor <- function(key = "am", collapse = FALSE,
                                  ignore_octave = FALSE){
  x <- scale_minor(key, ignore_octave = ignore_octave)
  tkey <- if(key_is_natural(key)) "g" else key
  x[4] <- transpose(x[4], 1, key = tkey)
  x[7] <- transpose(x[7], 1, key = tkey)
  if(collapse) x <- paste0(x, collapse = " ")
  a <- if(key_is_flat(tkey)) "flat" else "sharp"
  .asnw(x, accidentals = a)
}

#' @export
#' @rdname scale-helpers
scale_melodic_minor <- function(key = "am", descending = FALSE,
                                collapse = FALSE, ignore_octave = FALSE){
  x <- scale_minor(key, ignore_octave = ignore_octave)
  if(descending){
    x <- rev(x)
  } else {
    key <- if(key_is_natural(key)) "g" else key
    x[6:7] <- transpose(x[6:7], 1, key = key)
  }
  if(collapse) x <- paste0(x, collapse = " ")
  a <- if(key_is_flat(key)) "flat" else "sharp"
  .asnw(x, accidentals = a)
}

#' @export
#' @rdname scale-helpers
scale_jazz_minor <- function(key = "am", collapse = FALSE,
                             ignore_octave = FALSE){
  scale_melodic_minor(key, FALSE, collapse, ignore_octave)
}

#' @export
#' @rdname scale-helpers
scale_chromatic <- function(root = "c", collapse = FALSE, sharp = TRUE,
                            ignore_octave = FALSE){
  x <- ifelse(sharp, "c c# d d# e f f# g g# a a# b",
              "c d_ d e_ e f g_ g a_ a b_ b")
  y <- strsplit(x, " ")[[1]]
  if(!root %in% y) stop(paste("`root` is not one of:", x), call. = FALSE)
  idx <- match(root, y)
  if(idx != 1){
    y <- y[c(idx:length(y), 1:(idx - 1))]
    if(!ignore_octave) y <- .scale_set_octave(y, "tick")
  }
  if(collapse) y <- paste0(y, collapse = " ")
  .asnw(y)
}

#' Diatonic chords
#'
#' Obtain an ordered sequence of the diatonic chords for a given scale, as
#' triads or sevenths.
#'
#' @param root character, root note or starting position of scale.
#' @param scale character, a valid named scale, referring to one of the
#' existing \code{scale_*} functions.
#' @param type character, type of chord, triad or seventh.
#' @param collapse logical, collapse result into a single string ready for
#' phrase construction.
#'
#' @return character
#' @export
#'
#' @examples
#' scale_chords("c", "major")
#' scale_chords("a", "minor")
#' scale_chords("a", "harmonic minor")
#' scale_chords("a", "melodic minor")
#' scale_chords("a", "jazz minor")
#' scale_chords("a", "hungarian minor")
#'
#' scale_chords("c", "major", "seventh", collapse = TRUE)
#' scale_chords("a", "minor", "seventh", collapse = TRUE)
scale_chords <- function(root = "c", scale = "major",
                         type = c("triad", "seventh"), collapse = FALSE){
  type <- match.arg(type)
  s <- paste0("scale_", gsub(" ", "_", scale))
  .check_scale_fun(s)
  key <- if(scale == "major") root else paste0(root, "m")
  s <- do.call(s, list(key = key))
  x <- .diatonic_scale_chords(s, seq_along(s), type)
  if(collapse) x <- paste0(x, collapse = " ")
  .asnw(x)
}

.diatonic_scale_chords <- function(scale, deg, type){
  idx <- if(type == "triad") c(1, 3, 5) else c(1, 3, 5, 7)
  sapply(deg, function(x) paste0(note_shift(scale, x - 1)[idx], collapse = ""))
}

.scale_set_octave <- function(x, octaves){
  a <- if(any(grepl("_", x))) "flat" else "sharp"
  r <- chord_order(x)[1] - 1
  if(r != 0){
    if(r <= 3){
      x[1:r] <- sapply(x[1:r], transpose, n = -12, octaves = octaves,
                       accidentals = a)
    } else {
      n <- length(x)
      x[(r + 1):n] <- sapply(x[(r + 1):n], transpose, n = 12, octaves = octaves,
                             accidentals = a)
    }
  }
  x
}

#' Scale degrees and mappings
#'
#' These functions assist with mapping between scale degrees, notes and chords.
#'
#' Obtain the scale degree of a note in a supported scale with
#' \code{scale_degree}.
#' This function works on any noteworthy string. It ignores octave numbering.
#' Rests and any note not explicitly in the scale return \code{NA}. If
#' \code{deg} is greater than the number of degrees in the scale, it is
#' recycled, e.g., in C major 8 starts over as C.
#'
#' By default, flats and sharps checked strictly against the scale. Setting
#' \code{strict_accidentals = FALSE} will convert any flats or sharps present,
#' if necessary based on the combination of \code{key} signature and
#' \code{scale}. The chromatic scale is a special case where strict accidental
#' is always ignored.
#'
#' Not any arbitrary combination of valid \code{key} and valid \code{scale} is
#' valid. For example, \code{key = "am"} and \code{scale = "harmonic"} is
#' valid, but not with \code{key = "a"}.
#'
#' \code{note_in_scale} is a wrapper around \code{scale_degree}.
#' To check if full chords are diatonic to the scale, see
#' \code{\link{is_diatonic}}.
#'
#' The inverse of \code{scale_degree} is \code{scale_note}, for obtaining the
#' note associated with a scale degree.
#' This could be done simply by calling a \code{scale_*} function and indexing
#' its output directly, but this wrapper is provided to complement
#' \code{scale_degree}.
#' Additionally, it accepts the common Roman numeral input for the degree.
#' This can be with the \code{roman} class or as a character string.
#' Degrees return \code{NA} if outside the scale degree range.
#'
#' @param notes character, a string of notes.
#' @param deg integer, roman class, or character roman, the scale degree.
#' @param key character, key signature (or root note) for scale, depending on
#' the type of \code{scale}.
#' @param scale character, the suffix of a supported \code{scale_*} function.
#' @param use_root logical, use lowest pitch in chord. Otherwise yield an
#' \code{NA} in output.
#' @param strict_accidentals logical, whether representation must match key and
#' scale. See details.
#' @param naturalize logical, whether to naturalize any sharps or flats before
#' obtaiuning the scale degree.
#' @param roman logical, return integer scale degrees as Roman numerals.
#' @param collapse logical, collapse result into a single string ready for
#' phrase construction.
#' @param ... additional arguments passed to the scale function, e.g.,
#' \code{sharp = FALSE} for \code{scale_chromatic}.
#'
#' @return integer, or roman class if \code{roman = TRUE} for
#' \code{scale_degree}. character for \code{scale_note}.
#' @export
#' @name scale-deg
#' @seealso \code{\link{scale-helpers}}, \code{\link{is_diatonic}}
#'
#' @examples
#' scale_degree("r c, e3 g~ g s g# ceg")
#' note_in_scale("r c, e3 g~ g s g# ceg")
#'
#' scale_degree("c e g", roman = TRUE)
#'
#' scale_degree("c c# d_ e", key = "d")
#' scale_degree("c c# d_ e", key = "d", strict_accidentals = FALSE)
#'
#' scale_degree("c, e_3 g' f#ac#", use_root = FALSE)
#' scale_degree("c, e_3 g' f#ac#", naturalize = TRUE) # lowest chord pitch: c#
#'
#' scale_degree("c# d_ e_' e4 f f# g", key = "c#", scale = "chromatic")
#'
#' scale_note(1:3, key = "am")
#' scale_note(c(1, 3, 8), "d", collapse = TRUE)
#' all(sapply(list(4, "IV", as.roman(4)), scale_note) == "f")
#'
#' x <- "d dfa df#a f#ac#"
#' chord_degree(x, "d")
#' is_in_scale(x, "d")
scale_degree <- function(notes, key = "c", scale = "diatonic", use_root = TRUE,
                         strict_accidentals = TRUE, naturalize = FALSE,
                         roman = FALSE){
  .check_noteworthy(notes)
  s <- paste0("scale_", gsub(" ", "_", scale))
  .check_scale_fun(s)
  x <- .pitch_to_note(do.call(s, list(key)))
  if(note_has_flat(x)){
    a <- "flat"
  } else if(note_has_sharp(x)){
    a <- "sharp"
  } else {
    a <- if(key_is_sharp(key) | scale == "chromatic") "sharp" else "flat"
  }
  y <- .pitch_to_note(.uncollapse(notes))
  if(use_root) y <- .chord_root(y)
  y[y %in% c("r", "s")] <- NA_character_
  if(naturalize){
    x <- match(naturalize(y), x)
  } else {
    y2 <- .pitch_conform(y, accidentals = a)
    x <- match(y2, x)
    if(strict_accidentals & scale != "chromatic"){
      idx <- which(!(y == y2))
      if(length(idx)) x[idx] <- NA_integer_
    }
  }
  if(roman) x <- utils::as.roman(x)
  x
}

#' @export
#' @rdname scale-deg
scale_note <- function(deg, key = "c", scale = "diatonic", collapse = FALSE,
                       ...){
  s <- paste0("scale_", gsub(" ", "_", scale))
  .check_scale_fun(s)
  x <- .pitch_to_note(do.call(s, c(list(key), list(...))))
  deg <- if(is.character(deg)){
    as.integer(utils::as.roman(strsplit(deg, " ")[[1]]))
  } else {
    as.integer(deg)
  }
  if(any(deg < 1)) stop("`deg` should be >= 1.", call. = FALSE)
  deg <- deg %% length(x)
  x <- x[deg]
  if(collapse) x <- .asnw(paste0(x, collapse = " "))
  x
}

#' @export
#' @rdname scale-deg
note_in_scale <- function(notes, key = "c", scale = "diatonic", use_root = TRUE,
                          strict_accidentals = TRUE){
  x <- scale_degree(notes, key, scale, use_root, strict_accidentals,
                    naturalize = FALSE, roman = FALSE)
  idx <- .uncollapse(notes) %in% c("r", "s")
  x <- !is.na(x)
  if(any(idx)) x[idx] <- NA
  x
}

.check_scale_fun <- function(x){
  if(!exists(x, where = asNamespace("tabr"), mode = "function"))
    stop(paste0("`tabr::", x, "` is not an exported scale."), call. = FALSE)
}

#' @export
#' @rdname scale-deg
chord_degree <- function(notes, key = "c", scale = "diatonic",
                         strict_accidentals = TRUE, naturalize = FALSE,
                         roman = FALSE){
  purrr::map(.uncollapse(notes), ~{
    x <- .split_chords(.x)
    scale_degree(x, key, scale, FALSE, strict_accidentals, naturalize, roman)
  })
}

#' @export
#' @rdname scale-deg
is_in_scale <- function(notes, key = "c", scale = "diatonic",
                         strict_accidentals = TRUE){
  purrr::map_lgl(.uncollapse(notes), ~{
    x <- .split_chords(.x)
    all(note_in_scale(x, key, scale, FALSE, strict_accidentals))
  })
}

#' Check if notes and chords are diatonic
#'
#' Check if notes and chords are diatonic in a given key.
#'
#' This function is a wrapper around \code{\link{is_in_scale}}. To check if
#' individual notes are in a scale, see \code{\link{note_in_scale}}.
#'
#' @param notes character, a noteworthy string.
#' @param key character, key signature.
#'
#' @return logical
#' @export
#' @seealso \code{\link{is_in_scale}}
#'
#' @examples
#' is_diatonic("ceg ace ce_g", "c")
#' is_diatonic(c("r", "d", "dfa", "df#a"), "d")
is_diatonic <- function(notes, key = "c"){
  is_in_scale(notes, key, scale = "diatonic", strict_accidentals = TRUE)
}
