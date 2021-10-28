#' Rename items
#'
#' Rename items based on dic information.
#'
#' @param data A data frame
#' @param pattern A character string or vector of character strings defining a prefix.
#' May include the name of any dic attribute (e.g."item_label", "scale", "subscale", "subscale_2") or some shortcuts: "reverse", "label", or "name").
#' @param chars If not NULL, only the first n chars og the long label will be applied.
#' @param char_sep Character with seperator between prefix information.
#' @param char_weight Character vector of length two with signs for negative and positive weights.
#' @return A renamed data frame
#' @examples
#' ex_itrf %>%
#'   rename_items(pattern = c("reverse", "label")) %>%
#'   names()
#'
#' @export
rename_items <- function(data,
                         pattern = "item_label",
                         chars = NULL,
                         char_sep = "_",
                         char_weight = c("(-)", "(+)"),
                         char_prefix_label = ": ") {

  for (col in 1:ncol(data)) {
    if (is.null(attr(data[[col]], .opt$dic))) next
    new_label <- ""
    for(i in 1:length(pattern)) {
      pat <- pattern[i]
      tmp_label <- ""
      tmp_char_sep <- char_sep

      if (pat == c("reverse")) {
        tmp_label <- paste0(
          ifelse(
            dic_attr(data[[col]], .opt$weight) < 0,
            char_weight[1],
            char_weight[2])
        )
        tmp_char_sep <- ""
      }
      if (pat == "values")
        tmp_label <- paste0("(",
          paste0(dic_attr(data[[col]], .opt$values), collapse = ", "),
          ")"
        )

      if (pat == "value_labels")
        tmp_label <- paste0("(",
          paste0(
            dic_attr(data[[col]], .opt$value_labels)$value, " = ",
            dic_attr(data[[col]], .opt$value_labels)$label,
            collapse = "; "
          ), ")"
        )


      if (pat == "label") pat <- .opt$item_label
      if (pat == "name") pat <- .opt$item_name

      new_pat <- !(pat %in% c("reverse", "values", "value_labels"))
      if (new_pat) tmp_label <- dic_attr(data[[col]], pat)

      if (length(pattern) > i){
        if (pattern[i + 1] %in% c("label", "item_label"))
          tmp_char_sep <- char_prefix_label
      }

      #if (class(tmp_label) == "character")
      new_label <- paste0(new_label, tmp_label, tmp_char_sep)


    }
    new_label <- substr(new_label, 1, nchar(new_label) - nchar(tmp_char_sep))
    if (!is.null(chars)) new_label <- substring(new_label, 1, chars)
    names(data)[col] <- new_label
  }
  data
}

