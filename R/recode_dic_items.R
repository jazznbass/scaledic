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
#' @param prefix_label Prefix is added to the item label of a recoded item
#' @return A recoded data frame
#'
#' @export
recode_dic_items <- function(df,
                             prefix_label = "(recoded)") {

  if (inherits(df, "data.frame")) {
    out <- .recode_dic_items(df, prefix_label = prefix_label)
  }

  if (inherits(df, "dic")) {
    out <- .recode_dic_items(
      data.frame(dat = df), prefix_label = prefix_label
    )
    out$df <- out[["df"]][["dat"]]
  }


  return_messages(out$msg)
  out$df
}

.recode_dic_items <- function(df,
                              prefix_label = "[recoded]") {
  msg <- c()

  var_recodes <- opt("recodes")

  for(i in 1:ncol(df)) {
    recoding <- dic_attr(df[[i]], var_recodes)
    if (!has_info(recoding)) next

    msg <- c(msg, "Found recoding information and recoded values.")

    default <- NA
    id_default <- which(recoding[[1]] == ".default")
    if (length(id_default) > 0) {
      default <- recoding[[2]][[id_default]]
      recoding <- recoding[-id_default, ]
      if (identical(default, "NA")) default <- NA
    }
    .new <- rep(default, length(df[[i]]))
    class(.new) <- class(df[[i]])
    dic_attr(.new) <- dic_attr(df[[i]])

    for (j in 1:nrow(recoding)) {
      from <- recoding[[1]][j]
      to <- recoding[[2]][j]

      if (identical(to, "NA")) to <- NA

      if (dic_attr(df[[i]], "type") %in% opt("numerics")) {
        from <- as.numeric(from)
        to <- as.numeric(to)
      }
      .filter <- which(df[[i]] == from)
      .new[.filter] <- to
    }

    df[[i]] <- .new

    new_values <- unique(recoding[[2]])

    if (!is.na(default)) new_values <- c(new_values, default)
    if (suppressWarnings(!any(is.na(as.numeric(new_values))))) {
      new_values <- as.numeric(new_values)
    }

    dic_attr(df[[i]], "values") <- new_values
    dic_attr(df[[i]], "value_labels") <- NULL
    dic_attr(df[[i]], "item_label") <- paste(
      prefix_label, dic_attr(df[[i]], "item_label")
    )
    dic_attr(df[[i]], "recodes") <- NULL
  }

  list(df = df, msg = msg)

}
