#' Select items
#'
#' Selects a data frame with variables of a specific scale
#'
#' @param data A data frame with dic information
#' @param filter A logical expression for any dic attribute (e.g. scale == "ITRF" & subscale == "Int")
#' @param names_only If TRUE, variable names are returned instead of a data frame
#' @param index_only If TRUE, variable indices are returned instead of a data frame
#' @return A data frame, a vector with variable names or a vector with indices
#' @export
select_items <- function(data,
                         filter = NULL,
                         names_only = FALSE,
                         index_only = FALSE) {

  filter <- deparse(substitute(filter))

  id <- .get_index(data = data, filter = filter, class = "item", names = FALSE)
  if (names_only) return(names(data)[id])
  if (index_only) return(id)
  data[, id, drop = FALSE]
}
