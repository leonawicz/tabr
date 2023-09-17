.keydata <- data.frame(
  key = c("c", "g", "d", "a", "e", "b", "f#", "c#", "f", "b_", "e_", "a_", "d_",
          "g_", "c_",
          "am", "em", "bm", "f#m", "c#m", "g#m", "d#m", "a#m", "dm", "gm", "cm",
          "fm", "b_m", "e_m", "a_m"),
  sf = rep(c(NA, rep("sharp", 7), rep("flat", 7)), 2),
  nsf = rep(c(0L, 1:7, 1:7), 2L),
  major = rep(c(TRUE, FALSE), each = 15),
  c_am_rel_int = as.integer(
    rep(c(0, 7, 2, 9, 4, 11, 6, 1, 5, 10, 3, 8, 1, 6, 11), 2)),
  stringsAsFactors = FALSE
)

#' Key signatures
#'
#' Helper functions for key signature information.
#'
#' The `keys()` function returns a vector of valid key signature IDs. These IDs
#' are how key signatures are specified throughout `tabr`, including in the
#' other helper functions here via `key`. Like the other functions here,
#' `key_is_sharp()` and `key_is_flat()` are for *key signatures*, not single
#' pitches whose sharp or flat status is always self-evident from their notation.
#' Major and minor keys are also self-evident from their notation, but
#' `key_is_major()` and `key_is_minor()` can still be useful when programming.
#'
#' @param type character, defaults to `"all"`.
#' @param key character, key signature.
#'
#' @return character vector.
#' @export
#'
#' @examples
#' keys()
#' key_is_natural(c("c", "am", "c#"))
#' x <- c("a", "e_")
#' key_is_sharp(x)
#' key_is_flat(x)
#' key_n_sharps(x)
#' key_n_flats(x)
keys <- function(type = c("all", "sharp", "flat")){
  type <- match.arg(type)
  if(type == "all") return(.keydata$key)
  .keydata$key[!is.na(.keydata$sf) & .keydata$sf == type]
}

.keycheck <- function(key) if(any(!key %in% .keydata$key))
  stop("Invalid `key`. See `keys()`.", call. = FALSE)

#' @export
#' @rdname keys
key_is_natural <- function(key){
  .keycheck(key)
  is.na(.keydata$sf[match(key, .keydata$key)])
}

#' @export
#' @rdname keys
key_is_sharp <- function(key){
  .keycheck(key)
  x <- .keydata$sf[match(key, .keydata$key)]
  !is.na(x) & x == "sharp"
}

#' @export
#' @rdname keys
key_is_flat <- function(key){
  .keycheck(key)
  x <- .keydata$sf[match(key, .keydata$key)]
  !is.na(x) & x == "flat"
}

#' @export
#' @rdname keys
key_n_sharps <- function(key){
  .keycheck(key)
  purrr::map_int(key, ~ifelse(key_is_sharp(.x),
                              .keydata$nsf[.keydata$key == .x], 0L))
}

#' @export
#' @rdname keys
key_n_flats <- function(key){
  .keycheck(key)
  purrr::map_int(key, ~ifelse(key_is_flat(.x),
                              .keydata$nsf[.keydata$key == .x], 0L))
}

#' @export
#' @rdname keys
key_is_major <- function(key){
  .keycheck(key)
  .keydata$major[match(key, .keydata$key)]
}

#' @export
#' @rdname keys
key_is_minor <- function(key){
  .keycheck(key)
  !key_is_major(key)
}
