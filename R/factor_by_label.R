#' Turns a dic variable into a factor based on the value labels
#'
#' This function takes a vector with dic information and converts it into a factor
#' based on the value labels stored in the dic attributes.
#'
#' @param x A vector with dic information.
#' @param ordered If TRUE, the resulting factor will be ordered. Default is FALSE.
#' @return A factor with levels based on the value labels. If no value labels
#'   are present, an empty factor is returned.
#' @examples
#' # Apply a dictionary to data
#' dat_dic <- apply_dic(ex_scaledic_data, ex_scaledic_dic)
#' # Proportions of responses
#' dat_dic$rel_2 |> factor_by_label() |> table() |> prop.table()
#' # A cross table
#' table(factor_by_label(dat_dic$rel_1), dat_dic$gender)
#' @export
factor_by_label <- function(x, ordered) {
  labels <- dic_attr(x, "values")
  labels <- labels[which(!is.na(names(labels)))]
  out <- factor(x, levels = labels, labels = names(labels), ordered = ordered)
  class(out) <- c("dic", class(out))
  dic_attr(out) <- dic_attr(x)
  dic_attr(out, "type") <- "factor"
  dic_attr(out, "values") <- setNames(1:length(levels(out)), levels(out))
  dic_attr(out, "value_labels") <- data.frame(value = 1:length(levels(out)), label = levels(out))
  out
}
