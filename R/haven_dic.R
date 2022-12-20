#' haven labels to dic files
#'
#' @param data A data frame containing variables with haven labels
#' @param remove_haven_class If TRUE, haven labels are removed.
#' @return A data frame with dic information
#' @export
haven_dic <- function(data, remove_haven_class = FALSE) {
  for(i in 1:ncol(data)) {
    item_label <- attr(data[[i]], "label")
    value_labels <- attr(data[[i]], "labels")
    item_name <- names(data)[i]

    if (!is.null(item_label)) {
      if (remove_haven_class) {
        class(data[[i]]) <- class(data[[i]])[which(!class(data[[i]]) %in% c("haven_labelled", "vctrs_vctr"))]
      }
      if(is.null(attr(data[[i]], .opt$dic))) {
        data[[i]] <- dic(data[[i]])
        dic_attr(data[[i]], .opt$item_name) <- names(data)[i]
      }
      if (length(item_label) == 1)
        dic_attr(data[[i]], .opt$item_label) <- item_label
      if (length(item_label) > 1) {
        warning("Label for ", item_name, " has legnth > 1")
        dic_attr(data[[i]], .opt$item_label) <- item_name
      }
    }

    if (!is.null(value_labels)) {
      if(is.null(attr(data[[i]], .opt$dic))){
        data[[i]] <- dic(data[[i]])
        dic_attr(data[[i]], .opt$item_name) <- names(data)[i]
      }
      dic_attr(data[[i]], .opt$values) <- value_labels
    }

    dic_attr(data[[i]], .opt$item_name) <- item_name

    if (is.null(dic_attr(data[[i]], .opt$class)))
      dic_attr(data[[i]], .opt$class) <- "item"
  }
  data
}
