#' @importFrom vctrs vec_ptype2 vec_cast
NULL

#' @export
vec_ptype2.dic.dic <- function(x, y, ...) {
  x
}

#' @export
vec_cast.dic.dic <- function(x, to, ...) {
  x
}
