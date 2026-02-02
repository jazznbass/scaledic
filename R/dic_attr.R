#' Set and get dictionary attributes
#'
#' Get or set dictionary attributes of a vector or a data.frame.
#
#' @param x A vector or a data.frame.
#' @param var Attribute name. If missing, the full attribute list is returned.
#' @return Attribute value
#' @examples
#' # set dic attributes
#' x <- 1:5
#' dic_attr(x, "class") <- "item"
#' dic_attr(x, "item_label") <- "An example item"
#' dic_attr(x, "item_label")
#' dic_attr(x)
#' @export
dic_attr <- function(x, var) {
  if (missing(var)) {
    return(attr(x, opt("dic")))
  }
  out <- attr(x, opt("dic"))
  if (is.null(out)) return(NULL)
  if (var %in% names(.opt)) {
    out[[opt(var)]]
  } else {
    out[[var]]
  }

}

#' @rdname dic_attr
#' @param value set value.
#' @return Modified object with updated attribute.
#' @export
"dic_attr<-" <- function(x, var, value) {
  out <- dic_attr(x)
  if (is.null(out)) {
    attr(x, opt("dic")) <- list()
    out <- list()
  }

  if (missing(var)) {
    attr(x, opt("dic")) <- value
    return(x)
  }
  if (var %in% opt()) {
    out[[opt(var)]] <- value
  } else {
    out[[var]] <- value
  }

  attr(x, opt("dic")) <- out
  x
}
