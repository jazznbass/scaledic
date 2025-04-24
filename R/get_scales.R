#' Extracts a list of item names based on dic information
#'
#' @param data A data.frame with dic information.
#' @param ... A logical expression defining a selection definition or a name
#'   defining a dic attribute for automatic scale definition.
#' @return A (named) list with character vectors of item names.
#' @details This function is basically a wrapper around the `select_items(data =
#'   data, filter = ..., names_only = TRUE)` function. It takes multiple filter
#'   expressions for a single data frame and returns the item names. This is
#'   mostly helpful for functions that take multiple scale definitions like
#'   `alpha_table()`. If you provide a name instead of a logical expression,
#'   that name must be a dic attribute. A list of scales will be created based
#'   on all values of that attribute.
#' @examples
#' ## define individual scales
#' get_scales(ex_itrf,
#'   'APD' = subscale_2 == "APD",
#'   'OPP' = subscale_2 == "OPP",
#'   "SW" = subscale_2 == "SW",
#'   'AD' = subscale_2 == "AD"
#' )
#'
#' ## generate scale list based on all instances of an attribute
#' get_scales(ex_itrf, subscale_2)
#' @export
get_scales <- function(data, ...) {
  scales <- as.list(substitute(...()))
  if (length(scales) == 1 && inherits(scales[[1]], "name")) {
    return(.get_all_scales(data, as.character(scales[[1]])))
  }
  out <- list()
  for(i in seq_along(scales)) {
    out[[names(scales)[i]]] <- do.call(
      "select_items",
      c(data = list(data),
        filter = scales[[i]],
        names_only = TRUE
      )
    )
    if (length(out[[names(scales)[i]]]) == 0)
      warning("No items found for '", names(scales)[i], "'.")
  }
  out
}

.get_all_scales <- function(data, scale_attr) {
  ids <- which_dic(data)
  scales <- unlist(lapply(data[ids, ], function(x) dic_attr(x, scale_attr)))
  scales <- unique(scales[!is.na(scales)])
  scale_filters <- paste0(scale_attr, " == '", scales, "'")

  out <- list()

  for(i in seq_along(scale_filters)) {
    out[[scales[i]]] <- do.call(
      "select_items",
      list(data = data,
           filter = str2lang(scale_filters[[i]]),
        names_only = TRUE
      )
    )
  }
  out
}
