#' Impute missing values
#'
#' @param data A data frame
#' @param force_to_scale If TRUE, imputed values will be rounded and forced to the scale.
#' That is, a value below the scale's minimum or maximum will be set to the scale's minimum and maximum.
#' @inheritParams get_index
#'
#' @return A data frame with imputed data.
#' @export
impute_missing <- function(data, scale = NULL, subscale = NULL, subscale_2 = NULL, force_to_scale = TRUE) {
  id <- get_index(data, scale = scale, subscale = subscale, subscale_2 = subscale_2)
  if (!any(is.na(data[, id]))) {
    cat("No missing data.\n")
    return(data)
  }

  data[, id] <- amelia(data[, id], boot.type = "none", m = 1)$imputations[[1]]

  if (force_to_scale) {
    for (i in id) {
      type <- dic_attr(data[[i]], .opt$type)
      values <- dic_attr(data[[i]], .opt$values)
      if (!(type %in% c("integer", "float", "real")) || is.null(values)) next
      x <- data[[i]]
      max_values <- max(values)
      min_values <- min(values)
      if (type %in% c("integer")) x[] <- round(x)
      x[] <- ifelse(x > max_values, max_values, x)
      x[] <- ifelse(x < min_values, min_values, x)
      data[, i] <- x
    }
  }
  data
}
