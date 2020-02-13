#' Create haven labels and value labels from dic
#'
#' @param data A data frame containing dic information
#'
#' @return A data frame with haven labels and value labels
#' @export
dic_haven <- function(data) {
  for(i in .get_dic_items(data)) {
    attr(data[[i]], "label") <- dic_attr(data[[i]], .opt$item_label)
    attr(data[[i]], "labels") <- dic_attr(data[[i]], .opt$values)
  }
  data
}



