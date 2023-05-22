#' Recode Dictionary Items
#'
#' This function takes a data frame and recodes specified variables based on the
#' dictionary.
#'
#' @param df a data frame that contains variables to be recoded
#' @param var_recoding the name of a variable within the dictionary information
#' @param prefix Prefix is added to the item label of a recoded item
#'
#' @return A recoded data frame
#'
#' @export
recode_dic_items <- function(df, var_recoding = "scores", prefix = "(recoded)") {
  out <- .recode_dic_items(df, var_recoding, prefix = prefix)
  return_messages(out$msg)
  out$df
}

.recode_dic_items <- function(df, var_recoding = "scores", prefix = "[recoded]") {
  msg <- c()
  for(i in 1:ncol(df)) {
    recoding <- dic_attr(df[[i]], var_recoding)
    values_new <- dic_attr(df[[i]], "values")
    value_labels_new <- dic_attr(df[[i]], "value_labels")
    if (is.null(recoding)) next
    msg <- c(msg, "Found recoding information and recoded values.")
    recoding <- gsub(" ", "", recoding) |>
      strsplit(",") |>
      unlist() |>
      lapply(function(x) strsplit(trimws(x), "=") |> unlist())
    .new <- df[[i]]
    for (j in 1:length(recoding)) {
      from <- recoding[[j]][1]
      to <- recoding[[j]][2]
      if (dic_attr(df[[i]], "type") %in% opt("numerics")) {
        from <- as.numeric(from)
        to <- as.numeric(to)
      }
      .filter <- which(df[[i]] == from)
      .new[.filter] <- to
      .id <- which(values_new == from)
      if (length(.id) > 0) values_new[.id] <- to
      .id <- which(value_labels_new$value == from)
      if (length(.id) > 0) value_labels_new$value[.id] <- to
    }

    df[[i]] <- .new
    #browser()
    #.values <- lapply(recoding, function(x) as.numeric(unname(x[[2]]))) |> unlist()
    dic_attr(df[[i]], "values") <- values_new
    dic_attr(df[[i]], "value_labels") <- value_labels_new
    dic_attr(df[[i]], "item_label") <- paste(prefix, dic_attr(df[[i]], "item_label"))
  }

  list(df = df, msg = msg)

}
