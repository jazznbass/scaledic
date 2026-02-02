#' Get a dictionary attribute of one or more variables.
#'
#' Retrieves a specific dic attribute from all variables in a data frame.
#'
#' If an attribute is not present for a variable, NA is returned for that variable.
#' If duplicates is set to FALSE, only unique attribute values are returned.
#'
#' @param data A data frame with dic information.
#' @param attribute Name of label attribute to retrieve.
#' @param duplicates If FALSE, duplicated attributes will be removed from
#'   returned vector.
#' @return Vector of attributes. If an attribute is not present, NA is returned.
#' @examples
#' ex_itrf |>
#'   select_items(subscale == "Int") |>
#'   get_dic_attribute("item_label")
#' @export
get_dic_attribute <- function(data, attribute, duplicates = TRUE) {

  out <- unname(sapply(data, function(x) {
    x <- dic_attr(x, attribute)
    if (is.null(x)) x <- NA
    x
    })
  )
  if (!duplicates) out <- unique(out)

  out
}
