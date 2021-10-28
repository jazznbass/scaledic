#' (Deprecated) Select a scale
#'
#' Selects a data frame with variables of a specific scale
#'
#' @param data A data frame with dic information
#' @param filter A logical expression for any dic attribute (e.g. scale == "ITRF" & subscale == "Int")
#' @return A data frame
#' @export
select_scale <- function(data,
                         filter = NULL,
                         scale = NULL,
                         subscale = NULL,
                         subscale_2 = NULL) {

  warning("select_scale() is deprecated. Please use function select_items instead. E.g.: select_items(data, scale == 'itrf' & subscale == 'Int')")

  filter <- deparse(substitute(filter))
  if (!is.null(scale) || !is.null(subscale) || !is.null(subscale_2)) {
    filter <- .to_filter(scale = scale, subscale = subscale, subscale_2 = subscale_2)
  }

  id <- .get_index(data = data, filter = filter, class = "item")
  data[, id, drop = FALSE]
}
