#' Get label of a scale
#'
#' @param data A data frame with dic information
#' @param scale A scale definition (e.g. scale = subscale == "Int").
#' @param label Name of label attribute
#'
#' @return Labels for a given scale
#' @export
get_label <- function(data, scale, label) {
  scale <- deparse(substitute(scale))
  id <- .get_index(data = data, filter = scale, class = "item")
  dic_names <- sapply(data[id], function(x) dic_attr(x, label))
  dic_names <- unique(dic_names)
  #if (nrow(dic_names) > 1) stop("Multiple labels for given scale.")
  #if (nrow(dic_names) != 1) stop("Wrong scale definition.")
  dic_names
}
