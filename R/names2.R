#' Rename items
#' Set names of items as long labels and vice versa.
#'
#' @param data A data frame
#' @param chars If not NULL, only the first n chars og the long label will be applied.
#' @param short If TRUE, the sublabel will be set before the variable name.
#' @param id If set TRUE, the item id will be set in the variable name
#' @param reverse If set TRUE, weights will be set in the varibable name
#' @param scale Unused
#' @param sep Character with seperator.
#' @details names2item renames to the long label.
#' names2label renames to the short label.
#'
#' @return A renamed data frame
#' @export
names2item <- function(data, chars = NULL, short = FALSE, id = FALSE,
                       reverse = FALSE, scale = NULL, sep = "_") {
  for (i in 1:ncol(data)) {
    item_label <- dic_attr(data[[i]], .opt$item_label)
    if (!is.null(item_label)) {
      prefix <- ""
      if (short) prefix <- dic_attr(data[[i]], .opt$subscale)
      if (id) prefix <- paste0(prefix, dic_attr(data[[i]], .opt$index), sep = sep)
      if (reverse) prefix <- paste0(prefix, dic_attr(data[[i]], .opt$weight), sep = sep)
      if (short || id) prefix <- paste0(prefix, ":")
      item_label <- paste0(prefix, item_label)
      if (!is.null(chars)) item_label <- substring(item_label, 1, chars)
      names(data)[i] <- item_label
    }
  }
  data
}

#' @rdname names2item
names2label <- function(data) {
  for (i in 1:ncol(data)) {
    label <- dic_attr(data[[i]], .opt$item_label_short)
    if (!is.null(label)) names(data)[i] <- label
  }
  data
}
