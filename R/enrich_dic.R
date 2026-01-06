#' Enriches a data frame with dic information
#'
#' Adds dic information to all variables in a data frame that do not have a dic
#' attribute. This is useful when creating a dic from a data frame that has some variables
#' already enriched with dic information and some that are not.
#' The function uses the variable name as item_name and the variable label
#' (if present) or the variable name as item_label.
#'
#' @param dat A data frame.
#' @return A data frame with dic information added to all variables.
#' @examples
#' df <- data.frame(
#'   age = c(25, 30, 35),
#'   gender = c("M", "F", "M")
#' )
#' df$age <- new_dic(df$age, item_name = "age", item_label = "Age of respondent")
#' df_enriched <- enrich_dic(df)
#' attributes(df_enriched$gender)$dic |> str()
#' @keywords internal
#'
#' @export
enrich_dic <- function(dat) {

  for (i in seq_along(dat)) {
    if (!is.null(attr(dat[[i]], opt("dic")))) next

    item_label <- attr(dat[[i]], "label")
    if (is.null(item_label)) item_label <- names(dat)[i]
    item_name <- names(dat)[i]

    dat[[i]] <- new_dic(
      dat[[i]],
      item_name = item_name,
      item_label = item_label
    )

  }

  dat

}

