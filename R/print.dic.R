#' Print dic infos
#'
#' @param x A variable with dic infos
#' @param ... Further parameters for the print function
#'
#' @return Dic infos of x
#' @keywords internal
#' @export
print.dic <- function(x, ...) {
  args <- list(...)
  dic(x)
}
