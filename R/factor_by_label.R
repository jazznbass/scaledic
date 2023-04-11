#' Turns a dic variable into a factor based on the value labels
#'
#' @param x A vector with dic information.
#'
#' @return A factor
#' @examples
#' dat_dic <- apply_dic(ex_scaledic_data, ex_scaledic_dic)
#' # Proportions of responses
#' dat_dic$rel_2 |> factor_by_label() |> table() |> prop.table()
#' # A cross table
#' table(factor_by_label(dat_dic$rel_1), dat_dic$gender)
#' @export
factor_by_label <- function(x) {
  labels <- dic_attr(x, "values")
  id <- which(!is.na(names(labels)))
  labels[id] <- names(labels)[id]
  factor(x, labels = labels)
}
