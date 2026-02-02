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
#' @param integer_as_float If TRUE, type 'integer' will be handled as 'float'.
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
                         integer_as_float = TRUE) {

  init_messages(); on.exit(print_messages())

  if (!inherits(data, "data.frame")) data <- data.frame(data)

  id <- which_dic(data)
  name <- names(data)
  errors <- list()

  for (i in id) {

    if(dic_attr(data[[i]], "type") %in% "integer" && integer_as_float) {
      dic_attr(data[[i]], "type") <- "float"
    }

    id_invalid_values <- id_invalid_values(data[[i]], include_missing)

    x <- data[[i]][id_invalid_values]

    dic_attr(x) <- NULL
    attr(x, "label") <- NULL
    attr(x, "labels") <- NULL
    class(x) <- class(x)[class(x) != "dic"]
    names(x) <- id_invalid_values
    if (!(length(id_invalid_values) > 0)) x <- NULL
    errors[[name[i]]] <- x

    if (!is.null(replace) && length(id_invalid_values) > 0) {
      data[id_invalid_values, i] <- replace
    }
  }

  if (report && length(errors) > 0) {
    msg <- c()
    for(i in seq_along(errors)) {
      word <- if (length(errors[[i]]) > 1) "rows" else "row"
      msg <- c(msg, paste0(
        "'", names(errors)[[i]], "' is ",
        paste0(errors[[i]], collapse = ", "), " at ", word, " ",
        paste0(names(errors[[i]]), collapse = ", "),
        sep = ""
      ))

    }
    add_message(
      if (is.null(replace)) {
        "Found the following invalid values:\n  "
      } else {
        paste0("Replaced the following invalid values with ", deparse(replace), ":\n  ")
      },
      paste0(msg, collapse = "\n  ")
    )
  }
  if (report && length(errors) == 0) add_message("No errors found.\n")
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
#' @return A vector with indices of invalid values
id_invalid_values <- function(x, include_missing = FALSE) {

  id_invalid_values <- NULL

  values <- dic_attr(x, "values")
  missing <- dic_attr(x, "missing")
  type <- dic_attr(x, "type")
  if (!include_missing) missing <- NULL

  if (has_info(values) || has_info(missing)) {
    if (type %in% "integer" || is.na(type)) {
      id_invalid_values <- which(!(x %in% c(values, missing)) & !is.na(x))
    }

    if (type %in% c("float", "real", "numeric", "double", "")) {
      id_invalid_values <- which(
        (x > max(values) | x < min(values)) &
          !is.na(x) & !x %in% missing
      )
    }

  }

  if (type %in% "factor") {
    id_invalid_values <- which(
      !(x %in% levels(x)) &
        !is.na(x) &
        !x %in% missing
    )
  }

  if (type %in% c("character", "characters", "string")) {
    id_invalid_values <- NULL
  }

  id_invalid_values

}
