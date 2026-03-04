#' Subset a dic variable while preserving dic attributes
#'
#' This function allows subsetting of dic variables while preserving their dic attributes,
#' as well as any haven labels and value labels.
#'
#' @rdname subset_dic
#' @param x An  object of class dic.
#' @param i Elements to be selected.
#' @param ... further arguments passed to the subset function.
#' @return A dic variable
#' @keywords internal
#' @export
`[.dic` <- function(x, i = TRUE, ...) {
  cl <- class(x)
  dic <- attr(x, opt("dic"))
  haven_label <- attr(x, "label")
  haven_labels <- attr(x, "labels")

  class(x) <- setdiff(cl, "dic")

  if (missing(i)) {
    out <- x[...]
  } else {
    out <- x[i, ...]
  }

  class(out) <- cl
  attr(out, opt("dic")) <- dic
  attr(out, "label") <- haven_label
  attr(out, "labels") <- haven_labels

  out
}

#' @rdname subset_dic
#' @export
`[.dic<-` <- function(x, i, ..., value) {
  cl  <- class(x)
  dic <- attr(x, opt("dic"))
  haven_label <- attr(x, "label")
  haven_labels <- attr(x, "labels")

  class(x) <- setdiff(cl, "dic")

  if (missing(i)) {
    x <- do.call("[<-", c(list(x), list(...), list(value = value)))
  } else {
    x <- do.call("[<-", c(list(x), list(i), list(...), list(value = value)))
  }

  class(x) <- cl
  attr(x, opt("dic")) <- dic
  attr(x, "label") <- haven_label
  attr(x, "labels") <- haven_labels
  x
}
