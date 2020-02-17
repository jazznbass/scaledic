#' Select a scale
#' Selects a data frame with variables of a specific scale
#'
#' @inheritParams get_index
#' @return A data frame
#' @export
select_scale <- function(data, filter = NULL, scale = NULL, subscale = NULL, subscale_2 = NULL) {

  filter <- deparse(substitute(filter))
  if (!is.null(scale) || !is.null(subscale) || !is.null(subscale_2)) {
    filter <- .to_filter(scale = scale, subscale = subscale, subscale_2 = subscale_2)
  }

  id <- .get_index(data = data, filter = filter, class = "item")
  data[, id, drop = FALSE]
}
