#' Select items from a data frame based on dictionary attributes
#'
#' Selects a data frame with variables of a specific scale based on dic attributes.
#'
#' @details
#' This function allows you to filter and select variables from a data frame
#' based on their dictionary attributes. You can specify a logical expression
#' using any dic attribute (e.g., `scale`, `subscale`, `subscale_2`, etc.)
#' to filter the items you want to select. For example, to select all items that belong to the "ITRF" scale and the "Int" subscale,
#' you would use the filter expression `scale == "ITRF" & subscale == "Int"`.
#' This function is particularly useful for working with large datasets where you need to extract specific subsets of items based on their characteristics defined in the dictionary.
#' It returns either a data frame with the selected items, a vector of variable names, or a vector of indices, depending on the parameters you provide.
#'
#' @param data A data frame with dic information.
#' @param filter A logical expression for any dic attribute (e.g. `scale ==
#'   "ITRF" & subscale == "Int"`) to filter the items.
#' @param names_only If TRUE, variable names are returned instead of a data
#'   frame.
#' @param index_only If TRUE, variable indices are returned instead of a data
#'   frame.
#' @return A data frame, a vector with variable names, or a vector with indices.
#' @examples
#' ## select items belonging to the "ITRF" scale and "Int" subscale
#' selected_items <- select_items(
#'   data = ex_itrf,
#'   filter = scale == "ITRF" & subscale == "Int"
#'  )
#'  head(selected_items)
#'  ## get names of selected items
#'  item_names <- select_items(
#'   data = ex_itrf,
#'   filter = scale == "ITRF" & subscale == "Int",
#'   names_only = TRUE
#'  )
#'  print(item_names)
#'  ## get indices of selected items
#'  item_indices <- select_items(
#'   data = ex_itrf,
#'   filter = scale == "ITRF" & subscale == "Int",
#'   index_only = TRUE
#'  )
#'  print(item_indices)
#' @export
select_items <- function(data,
                         filter = NULL,
                         names_only = FALSE,
                         index_only = FALSE) {

  filter <- substitute(filter)

  id <- get_index_from_dic(data = data, filter = filter, class = "item", names = FALSE)
  if (names_only) return(names(data)[id])
  if (index_only) return(id)
  data[, id, drop = FALSE]
}
