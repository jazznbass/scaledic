#' Check values
#' Checks if values in variables are within the set difined in
#' the values and type dictionary attributes.
#'
#' @param data A data frame
#' @param replace Value which relaces unvalid values (e.g., NA).
#' @param return If TRUE, a data frame is returned with replaced values.
#' @param include_missing If TRUE, missing values (provided in the dic file)
#' will be considered as valid values.
#'
#' @return A data frame with replaced values if replaces is not NULL.
#' @export
#' @examples
#' dat <- apply_dic(ITRF, dic_ITRF)
#' check_values(dat, return = FALSE)
check_values <- function(data, replace = NULL, return = TRUE, include_missing = TRUE) {
  id <- which(sapply(data, function(x) !is.null(attr(x, .opt$dic))))
  name <- names(data)
  errors <- list()
  for (i in id) {
    values <- dic_attr(data[[i]], .opt$values)
    missing <- dic_attr(data[[i]], .opt$missing)
    if (include_missing) values <- c(values, missing)
    type <- dic_attr(data[[i]], .opt$type)

    ### strict checking integers otherwise use float

    if (type %in% "integer" || is.na(type)) {
      id_error <- which(!(data[[i]] %in% values) & !is.na(data[[i]]))
    }

    if (type %in% c("float", "real")) {
      id_error <- which(
        (data[[i]] > max(values) | data[[i]] < min(values)) & !is.na(data[[i]])
      )
    }

    if (type %in% "factor") {
      id_error <- which(!(data[[i]] %in% values) & !is.na(data[[i]]))
    }

    x <- data[[i]][id_error]
    names(x) <- id_error
    if (!(length(id_error) > 0)) x <- NULL
    errors[[name[i]]] <- x

    if (!is.null(replace)) data[id_error, i] <- replace
  }
  if (return) {
    return(data)
  }
  return(errors)
}
