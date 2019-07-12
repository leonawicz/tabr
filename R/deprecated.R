#' Deprecated
#'
#' Deprecated tabr functions.
#'
#' @export
#' @name tabr_dep
dup <- function(x, n = 1){
  .Deprecated("pn")
  pn(x, n)
}

#' @export
#' @rdname tabr_dep
glue <- function(...){
  .Deprecated("pc")
  pc(...)
}
