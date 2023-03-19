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
        .id <- which(!class(data[[i]]) %in% c("haven_labelled", "vctrs_vctr"))
        class(data[[i]]) <- class(data[[i]])[.id]
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

#' Create haven labels and value labels from dic
#'
#' @param data A data frame containing `dic` information
#' @param overwrite Logical. If `TRUE`, overwrites existing haven labels.
#'
#' @return A data frame with haven labels and value labels
#' @examples
#' ex_itrf_copy <- haven_dic(ex_itrf, remove_haven_class = TRUE)
#' ex_itrf_copy <- dic_haven(ex_itrf)
#' identical(ex_itrf, ex_itrf_copy)
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
