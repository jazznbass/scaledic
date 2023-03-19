#' Compute a new scaledic variable
#'
#' @param fun A function
#' @param data A data frame
#' @param ... dic parameters
#'
#' @return
#' @export
new_dic <- function(fun, data = NULL, ...) {
  fun <- deparse(substitute(fun))
  if (length(fun) > 1) fun <- paste0(fun, collapse = "")
  parameters <- list(...)
  if (!"item_name" %in% names(parameters)) {
    parameters <- c(list(item_name = "new_var"), parameters)
  }
  out <- with(data, eval(str2lang(fun)))
  out <- do.call("set_dic", c(list(data = out), parameters))
  out
}
