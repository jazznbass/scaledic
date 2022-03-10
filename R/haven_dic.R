#' haven labels to dic files
#'
#' @param data A data frame containing variables with haven labels
#'
#' @return A data frame with dic information
#' @export
haven_dic <- function(data) {
  for(i in 1:ncol(data)) {
    if (!is.null(attr(data[[i]], "label")))
      dic_attr(data[[i]], .opt$item_label) <- attr(data[[i]], "label")
    if (!is.null(attr(data[[i]], "labels")))
      dic_attr(data[[i]], .opt$values) <- attr(data[[i]], "labels")

    dic_attr(data[[i]], .opt$item_name) <- names(data)[i]

    if (is.null(dic_attr(data[[i]], .opt$class)))
      dic_attr(data[[i]], .opt$class) <- "item"
  }
  data
}
