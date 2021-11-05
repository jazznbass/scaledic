#' Impute missing values
#'
#' @param data A data frame
#' @param filter A logical expression for any dic attribute (e.g. scale == "ITRF" & subscale == "Int")
#' @param force_to_scale If TRUE, imputed values will be rounded and forced to the scale.
#' That is, a value below the scale's minimum or maximum will be set to the scale's minimum and maximum.
#' @param scale (deprecated) Character string.
#' @param subscale (deprecated) Character string.
#' @param subscale_2 (deprecated) Character string.
#' @return A data frame with imputed data.
#' @export
impute_missing <- function(data, filter = NULL, scale = NULL, subscale = NULL,
                           subscale_2 = NULL, force_to_scale = TRUE) {

  filter <- deparse(substitute(filter))
  if (!is.null(scale) || !is.null(subscale) || !is.null(subscale_2)) {
    filter <- .to_filter(scale = scale, subscale = subscale, subscale_2 = subscale_2)
  }


  data <- .impute_missing(data, filter, force_to_scale)

  data
}

.impute_missing <- function(data, filter, force_to_scale = TRUE) {

  id <- .get_index(data, filter, class = "item")
  if (!any(is.na(data[, id]))) {
    message("No missing data.\n")
    return(data)
  }
  cl <- class(data)
  data[, id] <- amelia(as.data.frame(data[, id]), boot.type = "none", m = 1)$imputations[[1]]
  class(data) <- cl

  if (force_to_scale) {
    for (i in id) {
      type <- dic_attr(data[[i]], .opt$type)
      values <- dic_attr(data[[i]], .opt$values)
      if (!(type %in% c("integer", "float", "real", "numeric", "double")) || is.null(values)) next
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
