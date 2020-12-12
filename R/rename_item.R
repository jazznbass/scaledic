#' Rename items
#' Rename items based on dic information.
#'
#' @param data A data frame
#' @param pattern A character string or vector of character strings defining a prefix.
#' May include "label", "scale", "subscale", "subscale_2", "reverse", "weight", or "name".
#' @param chars If not NULL, only the first n chars og the long label will be applied.
#' @param char_sep Character with seperator between prefix information.
#' @param char_weight Character vector of length two with signs for negative and positive weights.
#' @return A renamed data frame
#' @export
rename_item <- function(data, pattern = "label", chars = NULL, char_sep = ": ", char_weight = c("(-)", "(+)"), char_prefix_end = ": ") {

  for (col in 1:ncol(data)) {
    if (is.null(attr(data[[col]], .opt$dic))) next
    new_label <- ""
    for(pat in pattern) {
      tmp_label <- ""
      if (pat == "label") tmp_label <- dic_attr(data[[col]], .opt$item_label)
      if (pat %in% c("reverse", "weight")) {
        tmp_label <- paste0(
          ifelse(dic_attr(data[[col]], .opt$weight) < 0, char_weight[1], char_weight[2])
        )
      }
      if (pat == "scale") tmp_label <- dic_attr(data[[col]], .opt$scale)
      if (pat == "subscale") tmp_label <- dic_attr(data[[col]], .opt$subscale)
      if (pat == "subscale_2") tmp_label <- dic_attr(data[[col]], .opt$subscale_2)
      if (pat == "name") tmp_label <- dic_attr(data[[col]], .opt$item_name)

      if (class(tmp_label) == "character") new_label <- paste0(new_label, tmp_label, char_sep)
    }
    new_label <- substr(new_label, 1, nchar(new_label) - nchar(char_sep))
    if (!is.null(chars)) new_label <- substring(new_label, 1, chars)
    names(data)[col] <- new_label
  }
  data
}
