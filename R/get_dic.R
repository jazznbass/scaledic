#' Get dictionary information
#'
#' @param data A data frame
#'
#' @return A list with dic information
#' @export

get_dic <- function(data) {
  id <- .get_dic_items(data)
  out <- lapply(data[, id], function(x) attr(x, scaledic:::.opt$dic))
  out
}

