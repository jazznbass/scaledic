#' Get index of items from dic based on filter expression
#'
#' @param data A data frame
#' @param filter A character string or expression to filter the dic
#' @param names If TRUE, variable names are returned instead of indices
#' @param class If provided, class will be added to the filter expression
#' @return A vector with indices or names of variables adhering to the filter
#'    criteria.
#' @keywords internal
get_index_from_dic <- function(data, filter, names = TRUE, class = NULL) {

  if (is.call(filter) || is.expression(filter)) {
    filter <- deparse(filter)
  }
  if (!is.character(filter)) {
    abort("Internal error:",
         "filter must be a character string, call, or expression. Filter is of type ",
         class(filter))
  }

  if (!is.null(class)) filter <- paste0(filter, " & class == '", class, "'")

  dic <- extract_dic(data)

  id <- with(dic, eval(str2lang(filter)))
  out <- which(names(data) %in% dic[[opt("item_name")]][id])
  if (names) out <- names(data)[out]
  out
}
