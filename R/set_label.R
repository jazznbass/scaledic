#' Set dictionary information to variable
#'
#' @param data A data frame
#' @param vars string or vector with string of variable names
#' @param paramter Character string with dic parameter
#' @param values Vector of values of the same length as vars
#' @return A data frame with dic information
#' @export
set_label <- function(data, vars, parameter, values) {
  for(i in seq_along(vars)) {
    dic_attr(data[[vars[i]]], parameter) <- values[i]
  }
  data
}
