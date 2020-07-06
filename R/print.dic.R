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

  cat(first_line, "\n\n")

  if (!is.null(dic_attr(data, .opt$values))) {
    data_type <- dic_attr(data, .opt$type)
    cat("Data type is", data_type, "\n")
    if (data_type == "integer") {
      .string <- paste0(dic_attr(data, .opt$values), collapse = ", ")
      cat("Valid values: ", .string, "\n")
    }
    if (data_type == "float") {
      .string <- paste0(
        "From ", min(dic_attr(data, .opt$values)),
        " to ", max(dic_attr(data, .opt$values)), collapse = ""
      )
      cat("Valid values:", .string, "\n")
    }

  }

  if (!is.null(dic_attr(data, .opt$value_labels))) {
    print(dic_attr(data, .opt$value_labels), row.names = FALSE, right = FALSE)
  }



  class(data) <- class(data)[!class(data) %in% .opt$dic]
  attr(data, .opt$dic) <- NULL
  attr(data, "label") <- NULL
  attr(data, "labels") <- NULL
  cat("\n")
  print(data)

}
