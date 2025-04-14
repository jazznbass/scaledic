#' Recode Dictionary Items
#'
#' This function takes a data frame and recodes specified variables based on the
#' dictionary.
#'
#' This function is useful, when you want to recode the 'raw' values from a
#' vector in a data.frame based on recoding information provided in a
#' dictionary file. For example, you code the answers in a data frame that were
#' given to a task. And you have additional information in a dic-file that tells
#' you, which answer is correct (1) vs. false (0).
#'
#' @param df a data frame that contains variables to be recoded
#' @param var_recoding the name of a variable within the dictionary information
#' @param prefix Prefix is added to the item label of a recoded item
#'
#' @return A recoded data frame
#'
#' @export
recode_dic_items <- function(df,
                             var_recoding = "scores",
                             prefix_label = "(recoded)",
                             split = ";") {

  if (inherits(df, "data.frame")) {
    out <- .recode_dic_items(df, var_recoding, prefix = prefix)
  }

  if (inherits(df, "dic")) {
    out <- .recode_dic_items(
      data.frame(dat = df), var_recoding, prefix_label = prefix_label
    )
    out$df <- out[["df"]][["dat"]]
  }


  return_messages(out$msg)
  out$df
}

.recode_dic_items <- function(df,
                              var_recoding = "scores",
                              prefix_label = "[recoded]",
                              split = ";") {
  msg <- c()

  for(i in 1:ncol(df)) {
    recoding <- dic_attr(df[[i]], var_recoding)
    if (is.null(recoding) || is.na(recoding) || identical(recoding, "")) next
    values_new <- dic_attr(df[[i]], "values")
    value_labels_new <- dic_attr(df[[i]], "value_labels")

    msg <- c(msg, "Found recoding information and recoded values.")
    recoding <- string_to_list(recoding, split = split)

    if (!is.null(recoding[["default"]])) {
      if (identical(recoding[["default"]][[2]], "NA")) {
        .new <- rep(NA, length(df[[i]]))
      } else {
        .new <- rep(recoding[["default"]][[2]], length(df[[i]]))
      }

      class(.new) <- class(df[[i]])
      dic_attr(.new) <- dic_attr(df[[i]])
      recoding[["default"]] <- NULL
    } else {
      .new <- df[[i]]
    }

    for (j in 1:length(recoding)) {
      from <- recoding[[j]][1]
      to <- recoding[[j]][2]

      if (identical(to, "NA")) to <- NA

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

    dic_attr(df[[i]], "values") <- values_new
    dic_attr(df[[i]], "value_labels") <- value_labels_new
    dic_attr(df[[i]], "item_label") <- paste(
      prefix_label, dic_attr(df[[i]], "item_label")
    )
    dic_attr(df[[i]], "scores") <- NULL
  }

  list(df = df, msg = msg)

}
