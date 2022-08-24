
#' @export
rename_items_glue <- function(data,
                              pattern = "{item_label}",
                              max_chars = NULL,
                              char_reverse = c("(-)", "(+)")) {

  for (col in 1:ncol(data)) {
    if (is.null(attr(data[[col]], .opt$dic))) next




    reverse <- paste0(
      ifelse(
        dic_attr(data[[col]], .opt$weight) < 0,
        char_reverse[1],
        char_reveres[2])
    )

    label <- dic_attr(data[[col]], .opt$item_label)
    name <- dic_attr(data[[col]], .opt$item_name)

    values <- paste0(dic_attr(data[[col]], .opt$values), collapse = ",")

    value_labels <- paste0(
      dic_attr(data[[col]], .opt$value_labels)$value,
      " = ",
      dic_attr(data[[col]], .opt$value_labels)$label,
      collapse = "; "
    )

    new_label <- str_glue(pattern)
    if (!is.null(max_chars)) new_label <- substring(new_label, 1, max_chars)
    names(data)[col] <- new_label
  }
  data
}
