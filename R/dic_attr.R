#' Set and get dictionary attributes
#'
#' @param x data.frame or single variable
#' @param var Attribute
#'
#'
#' @return Attribute value
#' @export
dic_attr <- function(x, var) {
  out <- attr(x, .opt$dic)
  out[[var]]
}

#' @rdname dic_attr
#' @param value set value
#' @export
"dic_attr<-" <- function(x, var, value) {

  if (mode(x) == "list") {
    for(i in seq_along(x)) {
      dic_attr <- attr(x[[i]], .opt$dic)
      if (is.null(dic_attr)) dic_attr <- list()

      dic_attr[[var]] <- value
      attr(x[[i]], .opt$dic) <- dic_attr
    }
  } else {
    dic_attr <- attr(x, .opt$dic)
    if (is.null(dic_attr)) dic_attr <- list()

    dic_attr[[var]] <- value
    attr(x, .opt$dic) <- dic_attr
  }


  x
}
