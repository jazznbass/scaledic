#' Show variable with dic information
#'
#' @param A variable
#'
#' @return Prints content of varable with dic information added.
#' @export
dic <- function(data) {
  #slice <- slice[slice <= length(data)]
  cat(paste0(dic_attr(data, .opt$item_name), " (", dic_attr(data, .opt$scale_label), ")"), "\n")
  values <- dic_attr(data, .opt$values)
  labels <- names(dic_attr(data, .opt$values))
  cat(paste0(values, " = ", labels, collapse = "\n"), "\n")

  class(data) <- class(data)[!class(data) %in% .opt$dic]
  attr(data, .opt$dic) <- NULL
  print(data)
}
