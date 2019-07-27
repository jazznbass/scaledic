#' Impute missing values
#'
#' @param data A data frame
#' @inheritParams get_index
#'
#' @return A data frame with imputed data.
#' @export
impute_missing <- function(data, scale = NULL, subscale = NULL, subscale_2 = NULL) {
  id <- get_index(data, scale = scale, subscale = subscale, subscale_2 = subscale_2)
  if (!any(is.na(data[, id]))) {
    cat("No missing data.\n")
    return(data)
  }
  data[, id] <- amelia(data[, id], boot.type = "none", m = 1)$imputations[[1]]
  for (i in id) {
    type <- dic_attr(data[[i]], .opt$type)
    if (type %in% "factor") next
    x <- data[[i]]
    type <- dic_attr(data[[i]], .opt$type)
    values <- dic_attr(data[[i]], .opt$values)
    if (!is.null(values)) {
      max_values <- max(values)
      min_values <- min(values)

      if (type %in% c("integer")) x[] <- round(x)

      if (type %in% c("float", "real", "integer")) {
        x[] <- ifelse(x > max_values, max_values, x)
        x[] <- ifelse(x < min_values, min_values, x)
      }
      data[, i] <- x
    }
  }
  data
}
