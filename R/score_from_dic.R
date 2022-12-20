#' Automatic scale scoring from a dic file
#'
#' @param data A data frame with dic information
#' @param dic A dic file with scoring information
#' @param impute_values If TRUE, missing values are imputed before scoring.
#'
#' @return A data frame with added scale scores
#' @export

score_from_dic <- function(data,
                           dic,
                           impute_values = FALSE) {

  dic <- dic[!is.na(dic[[.opt$score_filter]]), ]

  if (nrow(dic) == 0) return(data)

  if (!.opt$score_function %in% names(dic)) dic[[.opt$score_function]] <- "mean"

  for(i in 1:nrow(dic)) {
    if (dic[[.opt$score_function]][i] == "mean") sum <- FALSE else sum <- TRUE
    new_var <- dic[[.opt$item_name]][i]
    filter <- dic[[.opt$score_filter]][i]
    label <- dic[[.opt$item_label]][i]

    if (impute_values) {
      data <- .impute_missing(data, filter)
    }

    data[[new_var]] <- .score_scale(data, filter = filter, label = label, FUN = NULL, sum = sum, bind = FALSE)

  }

  data

}

