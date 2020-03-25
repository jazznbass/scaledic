#' Check values
#' Checks if values in variables are within the set difined in
#' the values and type dictionary attributes.
#'
#' @param data A data frame
#' @param replace Value which relaces unvalid values (e.g., NA).
#' @param return If TRUE, a data frame is returned with replaced values.
#' @param report If TRUE, an overview of invalid values will be given.
#' @param include_missing If TRUE, missing values (provided in the dic file)
#' will be considered as valid values.
#' @return A data frame with replaced values if replaces is not NULL.
#' @export
#' @examples
#' dat <- apply_dic(ITRF, dic_ITRF)
#' check_values(dat, return = FALSE)
check_values <- function(data, replace = NULL, return = TRUE, report = FALSE, include_missing = FALSE, integer_as_double = TRUE) {

  if (!"data.frame" %in% class(data)) data <- data.frame(data)

  id <- .get_dic_items(data)
  name <- names(data)
  errors <- list()

  for (i in id) {
    values <- dic_attr(data[[i]], .opt$values)
    missing <- dic_attr(data[[i]], .opt$missing)
    #if (include_missing) values <- c(values, missing)
    if (!include_missing) missing <- NULL
    type <- dic_attr(data[[i]], .opt$type)

    ### strict checking integers otherwise use float

    if(type %in% "integer" && integer_as_double) type <- "float"

    if (type %in% "integer" || is.na(type)) {
      id_error <- which(!(data[[i]] %in% c(values, missing)) & !is.na(data[[i]]))
    }

    if (type %in% c("float", "real", "numeric", "")) {
      id_error <- which(
        (data[[i]] > max(values) | data[[i]] < min(values)) & !is.na(data[[i]]) & !data[[i]] %in% missing
      )

    }

    if (type %in% "factor") {
      id_error <- which(!(data[[i]] %in% levels(data[[i]])) & !is.na(data[[i]]) & !data[[i]] %in% missing
    )
    }

    if (type %in% c("character", "characters", "string")) {
      id_error <- NULL
    }

    x <- data[[i]][id_error]

    attr(x, .opt$dic) <- NULL
    attr(x, "label") <- NULL
    attr(x, "labels") <- NULL
    class(x) <- class(x)[class(x) != "dic"]
    names(x) <- id_error
    if (!(length(id_error) > 0)) x <- NULL
    errors[[name[i]]] <- x

    if (!is.null(replace)) data[id_error, i] <- replace
  }
  if (report) print(errors)
  if (return) {
    return(data)
  }
}
