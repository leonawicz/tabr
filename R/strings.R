#' Fold and unfold strings
#'
#' Fold or unfold a string on the expansion operator.
#'
#' These function work on arbitrary stings. They do not perform a noteworthy
#' check. This allows them to work for \code{info} strings as well. Make sure
#' your strings are properly formatted. \code{string_fold} always collapses the
#' output string as space-delimited.
#'
#' @param x character string, should be valid notes or note info such as beats.
#' @param n integer, minimum number of consecutive repeated values to warrant
#' folding, defaults to 3.
#'
#' @return character
#' @export
#'
#' @examples
#' time <- "8*3 16 4.. 16 16 2 2 4. 8 4 4 8*4 1"
#' x <- string_unfold(time)
#' x
#' string_fold(x) == time
#'
#' notes <- "a, b, c d e f g# a r ac'e' a c' e' c' r r r a"
#' x <- string_fold(notes)
#' x
#' string_unfold(x) == notes
string_unfold <- function(x){
  y <- .uncollapse(x)
  if(length(x) == 1) paste(y, collapse = " ") else y
}

#' @export
#' @rdname string_unfold
string_fold <- function(x, n = 3){
  y <- .uncollapse(x)
  y <- rle(y)
  purrr::map2_chr(y$values, y$lengths, ~{
    if(.y == 1) return(.x)
    if(.y < n) paste(rep(.x, .y), collapse = " ") else paste0(.x, "*", .y)
  }) %>% paste(collapse = " ")
}
