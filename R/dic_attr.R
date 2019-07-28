#' Set and get dictionary attributes
#'
#' @param x Variable
#' @param var Attribute
#' @param value Value
#'
#'
#' @return Attribute value
#' @export
dic_attr <- function(x, var) {
  out <- attr(x, .opt$dic)
  out[[var]]
}

#' @rdname dic_attr
#' @param values set value
"dic_attr<-" <- function(x, var, value) {
  dic_attr <- attr(x, .opt$dic)
  if (is.null(dic_attr)) dic_attr <- list()

  dic_attr[[var]] <- value
  attr(x, .opt$dic) <- dic_attr
  x
}
