#' Get index
#'
#' @param data A data frame
#' @param names If names is TRUE, a vector with variable names will be returned.
#' @param ... Any dic attribute name with a string vector (e.g., scale = "ITRF", subscale = c("Ext", "Int"), subscale = "ADB")
#'
#' @return A vector with indices adhering to the given attribute criteria.
#' @export


get_index <- function(data, names = TRUE, ...) {

  criteria <- list(...)
  if (any(!names(criteria) %in% .opt)) {
    stop("Unvalid dic attribute")
  }
  id <- list()
  for(i in 1:length(criteria)) {
    if (is.null(criteria[[i]])) next
    new_id <- which(data %>% map(~ dic_attr(.x, .opt[[names(criteria[i])]])) %in% criteria[[i]])
    id <- c(id, list(new_id))
  }

  id <- Reduce(intersect, id)
  id <- unlist(id)
  if (names) id <- names(data)[id]

  id
}
