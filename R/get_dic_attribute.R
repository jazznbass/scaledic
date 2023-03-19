#' Get a dictionary attribute of one or more variables.
#'
#' @param data A data frame with dic information
#' @param attribute Name of label attribute
#' @param duplicates If FALSE, duplicated attributes will be removed from
#'   returned vector.
#' @return Vector of attributes. If an attribute is not present, NA is returned.
#' @examples
#' select_items(ex_itrf, subscale == "Int") |> get_dic_attribute("item_label")
#' @export
get_dic_attribute <- function(data, attribute, duplicates = TRUE) {

  out <- unname(sapply(data, function(x) {
    x <- dic_attr(x, attribute)
    if (is.null(x)) x <- NA
    x
    })
  )
  if (!duplicates) out <- unique(out)
  #if (nrow(dic_names) > 1) stop("Multiple labels for given scale.")
  #if (nrow(dic_names) != 1) stop("Wrong scale definition.")
  out
}
