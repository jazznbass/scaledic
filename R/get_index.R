#' (Deprecated) Get index
#'
#' @param data A data frame
#' @param filter A logical expression for any dic attribute (e.g. scale == "ITRF" & subscale == "Int")
#' @param scale,subscale,subscale_2 deprecated
#' @param names If names is TRUE, a vector with variable names will be returned.
#' @param class deprecated
#' @return A vector with indices adhering to the given attribute criteria.
#' @export

get_index <- function(data, filter = NULL, scale = NULL, subscale = NULL,
                      subscale_2 = NULL, names = TRUE, class = NULL) {

  warning("get_index() is deprecated. Please use function select_items instead.")
  filter <- deparse(substitute(filter))
  if (!is.null(scale) || !is.null(subscale) || !is.null(subscale_2)) {
    filter <- .to_filter(scale = scale, subscale = subscale, subscale_2 = subscale_2)
  }

    .get_index(data = data, filter = filter, names = names, class = class)
}

