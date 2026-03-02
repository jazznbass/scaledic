#' Recode Dictionary Items
#'
#' This function takes a data frame and recodes variables based on the
#' `recodes` attribute stored in the dic attributes of a variable.
#'
#' For each variable in the data frame, if a `recodes` attribute is found,
#' the function creates a new variable where the values are recoded according
#' to the specified recoding rules. The original variable is replaced with the
#' recoded variable, and the item label is updated to indicate that the variable
#' has been recoded.
#'
#' @param df a data frame.
#' @param prefix_label Prefix is added to the item label of a recoded item.
#' @return A data frame with recoded variables.
#' @author Juergen Wilbert
#' @examples
#' q1 <- dic(
#'   x = c(1,1,2,3,1,3,4,4,3,2,4,5),
#'   item_name = "knowledge_1",
#'   item_label = "What is the capital of Germany?",
#'   type = "integer",
#'   weight = 1,
#'   values = "1:4",
#'   value_labels = "1 = Brussels; 2 = Hamburg; 3 = Bonn; 4 = Berlin",
#'   recodes = "1 = -1; 2 = 0; 3 = 0; 4 = 1; .default = NA"
#' )
#' q1
#' recode_dic_items(q1)
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

  out$df
}

.recode_dic_items <- function(df, prefix_label) {

  var_recodes <- opt("recodes")

  for(i in 1:ncol(df)) {
    recoding <- dic_attr(df[[i]], var_recodes)
    if (!has_info(recoding)) next

    notify("Found recoding information and recoded values.")

    default <- NA
    id_default <- which(recoding[["value"]] == ".default")
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

  list(df = df)

}
