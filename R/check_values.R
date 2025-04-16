#' Check values
#'
#' Checks if values in variables are valid according to the 'values' and 'type'
#' dictionary attributes.
#'
#' @param data A data frame
#' @param replace Value which relaces unvalid values (e.g., NA).
#' @param return If TRUE, a data frame is returned with replaced values.
#' @param report If TRUE, an overview of invalid values will be given.
#' @param include_missing If TRUE, missing values (provided as 'missing' in the
#'   dic file) will be considered as valid values.
#' @param integer_as_float If TRUE, type 'integer' will be handled as 'float'.
#'   That is, only values outside the minimum and the maximum of the provided
#'   valid values will be considered invalid.
#' @param check_type If TRUE, it will check if the class of a variable conflicts
#'   with the 'type' as defined in the dic information. When a type is numeric
#'   and the class is 'character' it will try to convert the class to a numeric
#'   class.
#' @return A data frame with replaced values if replaces is not NULL.
#' @export
#' @examples
#' check_values(ex_itrf, return = FALSE)
check_values <- function(data,
                         replace = NULL,
                         return = TRUE,
                         report = TRUE,
                         include_missing = FALSE,
                         integer_as_float = FALSE,
                         check_type = TRUE) {

  if (!inherits(data, "data.frame")) data <- data.frame(data)

  id <- which_dic(data)
  name <- names(data)
  errors <- list()

  for (i in id) {
    values <- dic_attr(data[[i]], "values")
    missing <- dic_attr(data[[i]], "missing")
    if (!include_missing) missing <- NULL
    type <- dic_attr(data[[i]], "type")

    ### strict checking integers otherwise use float

    if(type %in% "integer" && integer_as_float) type <- "float"

    if (type %in% opt("numerics") && check_type) {
      if ("character" %in% class(data[[i]])) {
        cat(
          names(data)[i],
          " has datatype character instead of numeric -> coerced to numeric\n"
        )
        .attr <- attributes(data[[i]])
        data[[i]] <- as.numeric(data[[i]])
        attributes(data[[i]]) <- .attr
        class(data[[i]]) <- c("dic", "numeric")
      }
    }

    if (type %in% "integer" || is.na(type)) {
      id_error <- which(!(data[[i]] %in% c(values, missing)) & !is.na(data[[i]]))
    }

    if (type %in% c("float", "real", "numeric", "")) {
      id_error <- which(
        (data[[i]] > max(values) | data[[i]] < min(values)) &
        !is.na(data[[i]]) & !data[[i]] %in% missing
      )
    }

    if (type %in% "factor") {
      id_error <- which(
        !(data[[i]] %in% levels(data[[i]])) &
        !is.na(data[[i]]) &
        !data[[i]] %in% missing
      )
    }

    if (type %in% c("character", "characters", "string")) {
      id_error <- NULL
    }

    x <- data[[i]][id_error]

    dic_attr(x) <- NULL
    attr(x, "label") <- NULL
    attr(x, "labels") <- NULL
    class(x) <- class(x)[class(x) != "dic"]
    names(x) <- id_error
    if (!(length(id_error) > 0)) x <- NULL
    errors[[name[i]]] <- x

    if (!is.null(replace) && length(id_error) > 0) {
      data[id_error, i] <- replace
    }
  }

  if (report && length(errors) > 0) {
    cat("Found the following invalid values:\n\n")
    for(i in seq_along(errors)) {
      cat("'", names(errors)[[i]], "'\n", sep = "")
      print(c("Row:" = "Value:", errors[[i]]), quote = FALSE)
      cat("\n")
    }
  }
  if (report && length(errors) == 0) cat("No errors found.\n")
  if (return) {
    return(data)
  }
}
