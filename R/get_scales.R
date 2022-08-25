#' Extracts a list of item names based on dic information
#'
#' @param data A data.frame with dic inforation.
#' @param ... selection definitions.
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
#' @export
get_scales <- function(data, ...) {
  scales <- as.list(substitute(...()))
  out <- list()
  for(i in seq_along(scales)) {
    out[[names(scales)[i]]] <- do.call(
      "select_items",
      c(data = list(data),
        filter = scales[[i]],
        names_only = TRUE
      )
    )
  }
  out
}

