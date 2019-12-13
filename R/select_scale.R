#' Select a scale
#' Selects a data frame with variables of a specific scale
#'
#' @inheritParams get_index
#' @return A data frame
#' @export
select_scale <- function(data, scale = NULL, subscale = NULL, subscale_2 = NULL) {
  id <- get_index(data = data, scale = scale, subscale = subscale, subscale_2 = subscale_2, class = "item")
  data[, id, drop = FALSE]
}
