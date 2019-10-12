#' Deploy dictionary information
#'
#' @param data A data frame
#' @param dic_list A list created by the get_dic() function.
#'
#' @return A data frame with added dic information from dic_list
#' @export

deploy_dic <- function(data, dic_list) {
  for(item in names(dic_list)) {
    if(item %in% names(data)) attr(data[[item]], .opt$dic) <- dic_list[[item]]
  }
  data
}
