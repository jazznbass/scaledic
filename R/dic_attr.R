#' Set and get dictionary attributes
#'
#' @param x Variable
#' @param var Attribute
#'
#'
#' @return Attribute value
#' @export
dic_attr <- function(x, var) {
  if (missing(var)) {
    return(attr(x, opt("dic")))
  }
  out <- attr(x, opt("dic"))
  if (var %in% names(.opt)) {
    out[[opt(var)]]
  } else {
    out[[var]]
  }

}

#' @rdname dic_attr
#' @param value set value
#' @export
"dic_attr<-" <- function(x, var, value) {
  dic_attr <- attr(x, opt("dic"))
  if (is.null(dic_attr)) dic_attr <- list()

  if (missing(var)) {
    attr(x, opt("dic")) <- values
    return(x)
  }
  if (var %in% names(.opt)) {
    dic_attr[[opt(var)]] <- value
  } else {
    dic_attr[[var]] <- value
  }
  attr(x, opt("dic")) <- dic_attr
  x
}
