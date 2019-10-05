# nolint start

#' Transpose pitch
#'
#' Transpose pitch by a number of semitones.
#'
#' This function transposes the pitch of notes in a valid character string. The string must be of the form passed to the \code{info} argument to \code{\link{phrase}}.
#'
#' Transposing is not done on a phrase object. The notes in a phrase object have already been transformed to LilyPond syntax and mixed with other potentially complex and variable information.
#' Transposing is intended to be done on a string of notes prior to passing it to \code{phrase}. It will work on strings that use either integer or tick mark octave numbering formats.
#' The transposed result will be a string with integer octave numbering.
#'
#' If \code{key} is provided, this helps ensure proper use of sharps vs. flats. Alternatively, you can simply provide \code{key = "sharp"} or \code{key = "flat"}. The exact key signature is not required, just more clear and informative for the user.
#' If not provided (\code{key = NA}), transposition lacks full information and simply defaults to sharping any resulting accidentals for positive \code{n} and flattening for negative \code{n}.
#' \code{n = 0} returns the input without any modification.
#'
#' The only element other pitch that occurs in a valid notes string is a rest, \code{"r"} or \code{"s"} (silent rest). Rests are ignored by transpose.
#'
#' The default \code{style} is to use tick style if no integers occur in \code{notes}. The \code{"tick"} and \code{"integer"} options force the respective styles. When integer style is returned, all \code{3}s are dropped since the third octave is the implicit center in LilyPond. \code{style = "strip"} removes any explicit octave information.
#'
#' @param notes character, a noteworthy string.
#' @param n integer, positive or negative number of semitones to transpose.
#' @param key character, the new key signature after transposing \code{notes}. See details.
#' @param style character, specify tick or integer style octave numbering in result. See details.
#'
#' @return character
#' @export
#'
#' @examples
#' transpose("a_3 b_4 c5", 0)
#' tp("a_3 b_4 c5", -1)
#' tp("a_3 b_4 c5", 1)
#' tp("a#3 b4 c#5", 11)
#' tp("a#3 b4 c#5", 12)
#' tp("a#3 b4 c#5", 13)
#' tp("a3 b4 c5", 2, key = "f")
#' tp("a3 b4 c5", 2, key = "g")
#' tp("a b' c''", 2, key = "flat")
#' tp("a, b ceg", 2, key = "sharp")
transpose <- function(notes, n = 0, key = NA, style = c("default", "tick", "integer", "strip")){
  if(inherits(notes, "phrase"))
    stop("`notes` must be a noteworthy string, not a phrase object.", call. = FALSE)
  .check_noteworthy(notes)
  n <- as.integer(n)
  if(n == 0 & is.na(key)) return(.asnw(notes))
  style <- match.arg(style)
  if(style == "default") style <- ifelse(length(grep("[,']", notes)), "tick", "integer")
  x <- purrr::map_chr(strsplit(notes, " ")[[1]], ~({
    .split_chord(.x) %>% sapply(.add_missing_3) %>% paste(collapse = "")
  }))
  if(all(x %in% c("r", "s"))) return(.asnw(paste(x, collapse = " ")))
  no_chords <- length(.split_chord(notes)) == length(x)
  if(no_chords){
    return(.asnw(.transpose(paste(x, collapse = " "), n, key, style)))
  }
  purrr::map_chr(x, ~({
    if(.x %in% c("r", "s")) return(.x)
    if(length(.split_chord(.x)) == 1) return(.transpose(.x, n, key, style))
    x <- paste(.split_chord(.x), collapse = " ")
    gsub(" ", "", .transpose(x, n, key, style))
  })) %>% paste(collapse = " ") %>% .asnw()
}

# nolint end

.add_missing_3 <- function(x){
  if(x %in% c("r", "s") || length(grep("[,'0-9]", x))) x else paste0(x, 3)
}

.transpose <- function(x, n = 0, key = NA, style){
  sharp <- c("c", "c#", "d", "d#", "e", "f", "f#", "g", "g#", "a", "a#", "b")
  flat <- c("c", "d_", "d", "e_", "e", "f", "g_", "g", "a_", "a", "b_", "b")
  x <- strsplit(x, " ")[[1]]
  idx <- grep("~", x)
  if(length(idx) > 0) x <- gsub("~", "", x)
  n_split <- purrr::map_int(x, ~({
    if(.x %in% c("r", "s")) return(2L)
    a <- which(!strsplit(.x, "")[[1]] %in% c(letters[1:7], "#", "_"))
    if(length(a)) as.integer(min(a)) else 2L
    })
  )
  x1 <- substr(x, 1, n_split - 1)
  x2 <- substring(x, n_split)
  x2 <- purrr::map_int(x2, ~({
    type <- if(is.na(suppressWarnings(as.integer(.x)))) "tick" else "int"
    if(type == "tick"){
      n <- nchar(.x)
      if(n == 0) return(3L)
      if(substr(.x, 1, 1) == ",") 3L - nchar(.x) else 3L + nchar(.x)
    } else {
      as.integer(.x)
    }
  })
  )

  notes <- abs(n) %% 12
  octaves <- sign(n) * ((abs(n) - notes) / 12)
  notes <- sign(n) * notes

  if(!(notes == 0 & is.na(key))){
    if(is.na(key)){
      y <- if(n > 0) sharp else flat # nolint start
    } else {
      if(key == "sharp") key <- "c#"
      if(key == "flat") key <- "d_"
      .keycheck(key)
      sf <- .keydata$sf[.keydata$key == key]
      if(is.na(sf) && n > 0){
        y <- sharp
      } else if(is.na(sf) && n < 0){
        y <- flat
      } else if(is.na(sf)){
        y <- sharp
      } else if(sf == "sharp"){
        y <- sharp
      } else {
        y <- flat # nolint end
      }
    }
    x1new <- purrr::map_chr(x1, ~({
      if(.x %in% c("r", "s")) return(.x)
      a <- if(.x %in% sharp) sharp else flat
      x <- rep(a, 3)[12 + match(.x, a) + notes]
      if(nchar(x) == 2){
        idx <- match(x, a)
        if(x != y[idx]) x <- y[idx]
      }
      x
    })
    )
  } else {
    x1new <- x1
  }

  cpass <- function(x){
    if(x %in% c("r", "s")) return(0L)
    idx <- unique(c(match(x, sharp), match(x, flat)))
    idx <- idx[!is.na(idx)]
    a <- 0
    if(notes > 0 && idx + notes > 12) a <- 1
    if(notes < 0 && idx + notes < 1) a <- -1
    as.integer(a)
  }

  pass <- purrr::map_int(x1, cpass)
  x2 <- x2 + octaves + pass
  x2[x1 %in% c("r", "s")] <- ""
  if(any(as.integer(x2[x2 != ""]) < 0)) stop("`Negative octave number not allowed in `tabr`.", call. = FALSE)
  if(length(idx) > 0) x2[idx] <- paste0(x2[idx], "~")
  x <- paste0(x1new, x2, collapse = " ")
  if(style == "tick") x <- .octave_to_tick(x)
  x <- gsub("3", "", x)
  if(style == "strip") x <- gsub("\\d|,|'", "", x)
  x
}

#' @export
#' @rdname transpose
tp <- transpose

.ly_transpose_defs <-
"#(define (naturalize-pitch p)
(let ((o (ly:pitch-octave p))
      (a (* 4 (ly:pitch-alteration p)))
      ;; alteration, a, in quarter tone steps,
      ;; for historical reasons
      (n (ly:pitch-notename p)))
 (cond
   ((and (> a 1) (or (eqv? n 6) (eqv? n 2)))
     (set! a (- a 2))
     (set! n (+ n 1)))
   ((and (< a -1) (or (eqv? n 0) (eqv? n 3)))
     (set! a (+ a 2))
     (set! n (- n 1))))
 (cond
   ((> a 2) (set! a (- a 4)) (set! n (+ n 1)))
   ((< a -2) (set! a (+ a 4)) (set! n (- n 1))))
 (if (< n 0) (begin (set! o (- o 1)) (set! n (+ n 7))))
 (if (> n 6) (begin (set! o (+ o 1)) (set! n (- n 7))))
 (ly:make-pitch o n (/ a 4))))

#(define (naturalize music)
(let ((es (ly:music-property music 'elements))
           (e (ly:music-property music 'element))
      (p (ly:music-property music 'pitch)))
          (if (pair? es)
          (ly:music-set-property!
          music 'elements
          (map naturalize es)))
      (if (ly:music? e)
        (ly:music-set-property!
           music 'element
         (naturalize e)))
         (if (ly:pitch? p)
         (begin
         (set! p (naturalize-pitch p))
         (ly:music-set-property! music 'pitch p)))
  music))

naturalizeMusic =
  #(define-music-function (m)
  (ly:music?)
(naturalize m))

"
