#' Subset a dic variable
#'
#' @param x A scdf object
#' @param i A case name from x
#' @param ... further arguments passed to the `[` function.
#'
#' @return A dic variable
#' @export
`[.dic`<- function(x, i = length(x), ...) {
  arg <- list(...)
  cl <- class(x)
  dic <- attr(x, .opt$dic)
  lab <- attr(x, "label")
  class(x) <- cl[!cl %in% "dic"] #cl[-1]
  out <- do.call("[", c(list(x), list(i), arg))
  class(out) <- cl
  attr(out, .opt$dic) <- dic
  attr(out, "label") <- lab
  out
}
