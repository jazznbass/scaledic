#' Deploy dictionary information
#'
#' @param data A data frame
#' @param dic_list A list created by the backup_dic() function.
#'
#' @return A data frame with added dic information from dic_list
#' @export
#' @keywords internal

deploy_dic <- function(data, dic_list) {
  for(item in names(dic_list)) {
    if(item %in% names(data)) dic_attr(data[[item]]) <- dic_list[[item]]
  }
  data <- dic_haven(data)
  data
}


#' Backup dictionary information
#'
#' @param data A data frame containing dic infromation.
#'
#' @return A list with dic information. This can later be writen back to a data
#'   frame with the deploy_dic() function.
#' @export
#' @keywords internal

backup_dic <- function(data) {
  id <- which_dic(data)
  out <- lapply(data[, id], function(x) dic_attr(x))
  out
}
