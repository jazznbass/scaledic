#' Subset a dic variable
#'
#' @param x An  object
#' @param i Elements to be selected
#' @param ... further arguments passed to the subset function.
#'
#' @return A dic variable
#' @keywords internal
#' @export
select_var <- function(x, i = length(x), ...) {
  arg <- list(...)
  dic <- attr(x, .opt$dic)
  lab <- attr(x, "label")
  out <- do.call("[", c(list(x), list(i), arg))
  attr(out, .opt$dic) <- dic
  attr(out, "label") <- lab
  out
}
