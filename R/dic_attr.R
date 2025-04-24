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
  out <- dic_attr(x)
  if (is.null(out)) out <- list()

  if (missing(var)) {
    attr(x, opt("dic")) <- value
    return(x)
  }
  if (var %in% names(.opt)) {
    out[[opt(var)]] <- value
  } else {
    out[[var]] <- value
  }
  attr(x, opt("dic")) <- out
  x
}
