#' Extracts a list of item names based on dic information
#'
#' @param data A data.frame with dic inforation.
#' @param ... selection definitions.
#' @param .variable character with name of the dic information to be returned. Default is "item_name".
#' @return A (named) list with string vectors of item names
#' @details This function is basically a wrapper around the `
#' select_items(data = data, filter = ..., names_only = TRUE)` function.
#' It takes mutiple filter expressions for a single data frame and returns the item names.
#' This is mostly helpfull for functions that take multiple scale definitions like `alpha_table()`
#' @examples
#' get_scales(ex_itrf,
#'   'APD' = subscale_2 == "APD",
#'   'OPP' = subscale_2 == "OPP",
#'   "SW" = subscale_2 == "SW",
#'   'AD' = subscale_2 == "AD"
#' )
#'
#' @export
get_scales <- function(data, ..., .variable = "item_name") {
  scales <- as.list(substitute(...()))
  out <- list()
  for(i in seq_along(scales)) {
    res <- do.call(
      "select_items",
      c(data = list(data),
        filter = scales[[i]]
        #names_only = TRUE
      )
    )
    out[[names(scales)[i]]] <- res %>%
      map(~ dic_attr(.x, .opt[[.variable]])) %>%
      unlist()

    if (length(out[[names(scales)[i]]]) == 0)
      warning("No items found for '", names(scales)[i], "'.")
  }
  out
}
