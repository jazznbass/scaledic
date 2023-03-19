#' Create haven labels and value labels from dic
#'
#' @param data A data frame containing dic information
#' @param overwrite Logical. If `TRUE`, overwrites existing haven labels.
#'
#' @return A data frame with haven labels and value labels
#' @export
dic_haven <- function(data, overwrite = TRUE) {
  for(i in .get_dic_items(data, items_only = FALSE)) {
    if (overwrite || is.null(attr(data[[i]], "label")))
      attr(data[[i]], "label") <- dic_attr(data[[i]], .opt$item_label)
    if (overwrite || is.null(attr(data[[i]], "labels")))
      attr(data[[i]], "labels") <- dic_attr(data[[i]], .opt$values)
  }
  data
}



