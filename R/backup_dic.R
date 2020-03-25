#' Backup dictionary information
#'
#' @param data A data frame containing dic infromation.
#'
#' @return A list with dic information. This can later be writen back to a data frame with the deploy_dic() function.
#' @export

backup_dic <- function(data) {
  id <- .get_dic_items(data)
  out <- lapply(data[, id], function(x) attr(x, scaledic:::.opt$dic))
  out
}

#' @rdname backup_dic
#' @export
get_dic <- function(data) {
  message("get_dic() is deprecated. Please use backup_dic() instead.")
  backup_dic(data)
}
