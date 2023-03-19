#' Backup and deploy dictionary information
#'
#' @details These are helper functions that is necessary when you apply a
#'   function that drops the attributes of a data.frame and thereby drops al
#'   *dic* information.
#'
#' @param data A data frame containing dic information.
#' @param dic_list A list created by the backup_dic() function.
#'
#' @return `backup_dic()` returns a list with dic information. This can later be
#'   written back to a data frame with `deploy_dic()`. `deploy_dic` returns a
#'   data.frame.
#' @examples
#' dic_backup <- backup_dic(ex_itrf)
#' ex_itrf_copy <- deploy_dic(ex_itrf, dic_backup)
#' identical(ex_itrf, ex_itrf_copy)
#' @export

backup_dic <- function(data) {
  id <- .get_dic_items(data)
  out <- lapply(data[, id], function(x) attr(x, .opt$dic))
  out
}

#' @export
#' @rdname backup_dic
deploy_dic <- function(data, dic_list) {
  for(item in names(dic_list)) {
    if(item %in% names(data)) attr(data[[item]], .opt$dic) <- dic_list[[item]]
  }
  data <- dic_haven(data)
  data
}

