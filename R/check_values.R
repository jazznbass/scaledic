#' Check values in data frame according to dic attributes
#'
#' Checks if values in variables are valid according to the 'values' and 'type'
#' dictionary attributes. Invalid values can be replaced with a specified value.
#' An overview of invalid values can be reported.
#'
#' This function only checks variables that have dic attributes. Variables
#' without dic attributes are ignored.
#'
#' By default, integer variables are treated as float variables. That is,
#' only values outside the provided valid values are considered invalid. If
#' `integer_as_float` is set to FALSE, only values not included in the provided
#' valid values are considered invalid.
#'
#' If `include_missing` is set to TRUE, values provided as 'missing' in the dic
#' file are considered valid values.
#'
#' @param data A data frame.
#' @param replace Value which replaces invalid values (e.g., NA). If NULL, no
#'  replacement is done.
#' @param return If TRUE, a data frame is returned with replaced values. If FALSE,
#'  no data frame is returned.
#' @param report If TRUE, an overview of invalid values will be given.
#' @param include_missing If TRUE, missing values (provided as 'missing' in the
#'   dic file) will be considered as valid values. If FALSE, missing values will be
#'   considered as invalid values.
#' @param integer_as_numeric If TRUE, type 'integer' will be handled as 'numeric'.
#'   That is, only values outside the minimum and the maximum of the provided
#'   valid values will be considered invalid. If FALSE, only values not included
#'   in the provided valid values will be considered invalid.
#' @return A data frame with replaced values if `replaces` is not NULL.
#' @export
#' @examples
#' check_values(ex_itrf, return = FALSE)
check_values <- function(data,
                         replace = NULL,
                         return = TRUE,
                         report = TRUE,
                         include_missing = FALSE,
                         integer_as_numeric = TRUE) {

  #if (!inherits(data, "data.frame")) data <- data.frame(data)

  for (i in which_dic(data)) {

    id_invalid_values <- id_invalid_values(
      data[[i]],
      include_missing,
      integer_as_numeric
    )
    if (report && length(id_invalid_values) > 0) {
      notify(
        names(data)[i], "' invalid at ",
        if_one(id_invalid_values, "row ", "rows "),
        paste0(id_invalid_values, collapse = ", "),
        " (is ", paste0(data[[i]][id_invalid_values], collapse = ", "), ")",
        if (!is.null(replace)) paste0(" -> set as ", deparse(replace))
      )
    }
    if (!is.null(replace) && length(id_invalid_values) > 0) {
      data[id_invalid_values, i] <- replace
    }
  }

  if (return) {
    return(data)
  } else {
    return(invisible(NULL))
  }
}

#' Identify invalid values in a vector according to dic attributes
#' @keywords internal
#' @param x A dic vector
#' @param include_missing If TRUE, missing values (provided as 'missing' in the
#'  dic file) will be considered as valid values. If FALSE, missing values will
#'  be considered as invalid values.
#' @param integer_as_numeric If TRUE, type 'integer' will be handled as 'numeric'.
#' @return A vector with indices of invalid values
id_invalid_values <- function(x, include_missing = FALSE, integer_as_numeric = TRUE) {

  id_invalid_values <- NULL

  values <- dic_attr(x, "values")
  missing <- dic_attr(x, "missing")
  type <- dic_attr(x, "type")
  if (integer_as_numeric && identical(type, "integer")) type <- "numeric"
  if (!include_missing) missing <- NULL

  if (has_info(values) || has_info(missing)) {
    if (type %in% "integer" || is.na(type)) {
      id_invalid_values <- which(!(x %in% c(values, missing)) & !is.na(x))
    }

    if (type %in% c("float", "real", "numeric", "double")) {
      id_invalid_values <- which(
        (x > max(values) | x < min(values)) &
          !is.na(x) & !x %in% missing
      )
    }

  }


  if (type %in% "factor") {
    id_invalid_values <- which(
      !(x %in% levels(x)) & !is.na(x) & !x %in% missing
    )
  }

  if (type %in% c("character", "characters", "string")) {
    id_invalid_values <- NULL
  }

  id_invalid_values

}
