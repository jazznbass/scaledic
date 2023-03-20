#' Set and get dictionary attributes
#'
#' @param x Variable
#' @param var Attribute
#'
#'
#' @return Attribute value
#' @export
dic_attr <- function(x, var) {
  out <- attr(x, opt("dic"))
  out[[opt(var)]]
}

#' @rdname dic_attr
#' @param value set value
#' @export
"dic_attr<-" <- function(x, var, value) {
  dic_attr <- attr(x, .opt$dic)
  if (is.null(dic_attr)) dic_attr <- list()

  dic_attr[[var]] <- value
  attr(x, .opt$dic) <- dic_attr
  x
}
