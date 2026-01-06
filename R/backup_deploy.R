#' Backup and deploy all dictionary information in a data frame
#'
#' These are workaround functions that store and retrieve all dic information
#' within a data frame. This is necessary when a function drops the attributes of
#' a vector.
#'
#' @param data A data frame.
#' @param dic_list A list created by the backup_dic() function.
#'
#' @return A data frame with added dic information from dic_list.
#'
#' @keywords internal
#' @examples
#' ## create a copy of ex_itrf and strip all dic information
#' copy_ex_itrf <- ex_itrf
#' copy_ex_itrf[] <- lapply(copy_ex_itrf, function(x) {attr(x, "dic") <- NULL; x})
#' attr(copy_ex_itrf, "dic") <- NULL
#'
#' ## backup dic information from ex_itrf and deploy to copy_ex_itrf
#' dic_list <- backup_dic(ex_itrf)
#' copy_ex_itrf <- deploy_dic(copy_ex_itrf, dic_list)
#'
#' ## test that both are identical
#' identical(ex_itrf, copy_ex_itrf)
#' @export
deploy_dic <- function(data, dic_list) {
  attr(data, opt("dic")) <- dic_list$df
  dic_list <- dic_list$items
  for(item in names(dic_list)) {
    if(item %in% names(data)) dic_attr(data[[item]]) <- dic_list[[item]]
  }
  data <- dic_haven(data)
  data
}

#' @rdname deploy_dic
#' @returns A list containing all dic information from the data frame.
#' @export
backup_dic <- function(data) {
  id <- which_dic(data, items_only = FALSE)
  out <- lapply(data[, id], function(x) dic_attr(x))
  list(items = out, df = attr(data, opt("dic")))
}
