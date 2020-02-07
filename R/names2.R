#' Rename items
#' Set names of items as long labels and vice versa.
#'
#' @param data A data frame
#' @param chars If not NULL, only the first n chars og the long label will be applied.
#' @param prefix A character string or vector of character strings defining a prefix.
#' May include "scale", "subscale", "subscale2", "index", "reverse", "weight", or "label".
#' @param char_sep Character with seperator between prefix information.
#' @param char_weight Character vector of length two with signs for negative and positive weights.
#' @param char_prefix_end Character with seperator between prefi and item.
#' @details names2item renames to the long label.
#' names2label renames to the short label.
#'
#' @return A renamed data frame
#' @export
names2item <- function(data, chars = NULL, prefix = "", char_sep = "_", char_weight = c("(-)", "(+)"), char_prefix_end = ": ") {

  for (i in 1:ncol(data)) {
    item_name <- dic_attr(data[[i]], .opt$item_name)
    if (!is.null(item_name)) {
      item_prefix <- ""
      if (any(c("reverse", "weight") %in% prefix))
        item_prefix <- paste0(
          item_prefix,
          ifelse(dic_attr(data[[i]], .opt$weight) < 0, char_weight[1], char_weight[2])
        )
      if ("scale" %in% prefix) item_prefix <- paste0(item_prefix, dic_attr(data[[i]], .opt$scale), char_sep)
      if ("subscale" %in% prefix) item_prefix <- paste0(item_prefix, dic_attr(data[[i]], .opt$subscale), char_sep)
      if ("subscale2" %in% prefix) item_prefix <- paste0(item_prefix, dic_attr(data[[i]], .opt$subscale_2), char_sep)
      if ("index" %in% prefix) item_prefix <- paste0(item_prefix, dic_attr(data[[i]], .opt$index), char_sep)
      if ("label" %in% prefix) item_prefix <- paste0(item_prefix, dic_attr(data[[i]], .opt$item_label), char_sep)

      if (item_prefix != "") {
        item_prefix <- paste0(substring(item_prefix, 1, nchar(item_prefix) - length(char_sep)), char_prefix_end)
      }

      item_name <- paste0(item_prefix, item_name)
      if (!is.null(chars)) item_name <- substring(item_name, 1, chars)
      names(data)[i] <- item_name
    }
  }
  data
}

#' @rdname names2item
names2label <- function(data) {
  for (i in 1:ncol(data)) {
    label <- dic_attr(data[[i]], .opt$item_label)
    if (!is.null(label)) names(data)[i] <- label
  }
  data
}
