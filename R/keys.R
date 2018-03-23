.keydata <- data.frame(
  key = c("c", "g", "d", "a", "e", "b", "f#", "c#", "f", "b_", "e_", "a_", "d_", "g_", "c_",
          "am", "em", "bm", "f#m", "c#m", "g#m", "d#m", "a#m", "dm", "gm", "cm", "fm", "b_m", "e_m", "a_m"),
  sf = rep(c(NA, rep("sharp", 7), rep("flat", 7)), 2),
  nsf = rep(c(0L, 1:7, 1:7), 2L),
  major = rep(c(TRUE, FALSE), each = 15), stringsAsFactors = FALSE
)

#' Key signatures
#'
#' Helper functions for key signature information.
#'
#' The \code{keys} function returns a vector of valid key signature IDs. These IDs are how key signatures are specified throughout \code{tabr}, including in the other helper functions here via \code{key}.
#' Like the other functions here, \code{is_sharp} and \code{is_flat} are for \emph{key signatures}, not single pitches whose sharp or flat status is always self-evident by its notation.
#'
#' @param type character, defaults to \code{"all"}.
#' @param key character, key signature.
#'
#' @return character vector.
#' @export
#'
#' @examples
#' keys()
#' is_natural(c("c", "am", "c#"))
#' x <- c("a", "e_")
#' is_sharp(x)
#' is_flat(x)
#' n_sharps(x)
#' n_flats(x)
keys <- function(type = c("all", "sharp", "flat")){
  type <- match.arg(type)
  if(type == "all") return(.keydata$key)
  .keydata$key[!is.na(.keydata$sf) & .keydata$sf == type]
}

.keycheck <- function(key) if(any(!key %in% .keydata$key)) stop("Invalid `key`. See `keys`.")

#' @export
#' @rdname keys
is_natural <- function(key){
  .keycheck(key)
  is.na(.keydata$sf[match(key, .keydata$key)])
}

#' @export
#' @rdname keys
is_sharp <- function(key){
  .keycheck(key)
  x <- .keydata$sf[match(key, .keydata$key)]
  !is.na(x) & x == "sharp"
}

#' @export
#' @rdname keys
is_flat <- function(key){
  .keycheck(key)
  x <- .keydata$sf[match(key, .keydata$key)]
  !is.na(x) & x == "flat"
}

#' @export
#' @rdname keys
n_sharps <- function(key){
  .keycheck(key)
  purrr::map_int(key, ~ifelse(is_sharp(.x), .keydata$nsf[.keydata$key == .x], 0L))
}

#' @export
#' @rdname keys
n_flats <- function(key){
  .keycheck(key)
  purrr::map_int(key, ~ifelse(is_flat(.x), .keydata$nsf[.keydata$key == .x], 0L))
}
