#' Print dic infos
#'
#' @param x A variable with dic infos
#' @param ... Further parameters for the print function
#'
#' @return Dic infos of x
#' @export
print.dic <- function(x, ...) {
  data <- x
  first_line <- paste0(dic_attr(data, .opt$item_label))
  if (!is.null(dic_attr(data, .opt$scale_label)))
    first_line <- paste0(first_line, " (", dic_attr(data, .opt$scale_label), ")", collapse = "")

  if (dic_attr(data, .opt$class) == "score")
    first_line <- paste0(
      dic_attr(data, .opt$item_label), " (scale ", dic_attr(data, .opt$score_function), ")\n",
      "Scale definition: ", dic_attr(data, .opt$score_filter), "\n",
      collapse = ""
    )

  cat(first_line, "\n")

  if (!is.null(dic_attr(data, .opt$values))) {
    values <- dic_attr(data, .opt$values)
    labels <- names(dic_attr(data, .opt$values))
    cat(paste0(values, " = ", labels, collapse = "\n"), "\n")
  }

  class(data) <- class(data)[!class(data) %in% .opt$dic]
  attr(data, .opt$dic) <- NULL
  attr(data, "label") <- NULL
  attr(data, "labels") <- NULL
  print(data)

}
