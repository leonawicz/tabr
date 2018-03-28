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
#' @param notes character, a valid string of notes, the type passed to \code{phrase}.
#' @param n integer, positive or negative number of semitones to transpose.
#' @param key character, the new key signature after transposing \code{notes}. See details.
#' @param style character, specify tick or integer style octave numbering in result. The default is to use integer style if any integers occur in \code{notes}. The other two options force the respective styles.
#' @param ... arguments passed to transpose.
#'
#' @return a character string.
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
#' tp("a, b c'", 2, key = "sharp")
transpose <- function(notes, n = 0, key = NA, style = c("default", "tick", "integer")){
  style <- match.arg(style)
  x <- notes
  if(style == "default"){
    style <- ifelse(length(grep("[0-9]", x)), "integer", "tick")
  }
  if(!inherits(x, "character")) stop("`notes` must be a valid character string of notes.")
  if(inherits(x, "phrase"))
    stop("`notes` must be a valid character string of notes, not a phrase object.")
  n <- as.integer(n)
  if(n == 0) return(x)
  sharp <- c("c", "c#", "d", "d#", "e", "f", "f#", "g", "g#", "a", "a#", "b")
  flat <- c("c", "d_", "d", "e_", "e", "f", "g_", "g", "a_", "a", "b_", "b")
  x <- strsplit(x, " ")[[1]]
  idx <- grep("~", x)
  if(length(idx) > 0) x <- gsub("~", "", x)
  n_split <- purrr::map_int(x, ~({
    if(.x == "r") return(2L)
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
      if(substr(.x, 1, 1) == ","){
        direction <- "down"
      } else if(substr(.x, 1, 1) == "'"){
        direction <- "up"
      } else {
        stop("`notes` is not a valid string of notes.")
      }
      if(direction == "up") 3L + nchar(.x) else 3L - nchar(.x)
    } else {
      as.integer(.x)
    }
  })
  )

  notes <- abs(n) %% 12
  octaves <- sign(n) * ((abs(n) - notes) / 12)
  notes <- sign(n) * notes

  if(notes != 0){
    if(is.na(key)){
      y <- if(n > 0) sharp else flat # nolint start
    } else {
      if(key == "sharp") key <- "c#"
      if(key == "flat") key <- "d_"
      if(!key %in% .keydata$key)
        stop(cat("Invalid `key`. Options are:\n", paste0(.keydata$key, collapse = ", "), ".\n"))
      sf <- .keydata$sf[.keydata$key == key]
      if(is.na(sf) && n > 0){
        y <- sharp
      } else if(is.na(sf) && n < 0){
        y <- flat
      } else if(sf == "sharp"){
        y <- sharp
      } else {
        y <- flat # nolint end
      }
    }
    x1new <- purrr::map_chr(x1, ~({
      if(.x == "r") return("r")
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
    if(x == "r") return(0L)
    idx <- unique(c(match(x, sharp), match(x, flat)))
    idx <- idx[!is.na(idx)]
    a <- 0
    if(notes > 0 && idx + notes > 12) a <- 1
    if(notes < 0 && idx + notes < 1) a <- -1
    as.integer(a)
  }

  pass <- purrr::map_int(x1, cpass)
  x2 <- x2 + octaves + pass
  x2[x1 == "r"] <- ""
  if(any(as.integer(x2[x2 != ""]) < 0)) stop("`Negative octave number not allowed in `tabr`.")
  if(length(idx) > 0) x2[idx] <- paste0(x2[idx], "~")
  x <- paste0(x1new, x2, collapse = " ")
  if(style == "tick") x <- .octavesub(x)
  x
}

#' @export
#' @rdname transpose
tp <- function(...) transpose(...)

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
