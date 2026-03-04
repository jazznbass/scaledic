#' Replace Missing values
#'
#' This function replaces missing values in a data frame based on the
#' missing value definitions provided in the dic attributes of the variables.
#' Variables without dic attributes are ignored.
#'
#' @param data A data frame
#' @param replace Replace value for missing values (default is NA).
#' @param report If TRUE, proportion of missing values is printed.
#' @return A data frame with replaced missing values.
#' @export
replace_missing <- function(data, replace = NA, report = TRUE) {
  

  id <- which_dic(data)
  var_names <- names(data)

  for (i in id) {
    missing_values <- dic_attr(data[[i]], opt("missing"))
    if (!has_info(missing_values)) next
    id_missing <- which(data[[i]] %in% missing_values)
    if (report && length(id_missing)) {
      notify(
        "Replaced ", length(id_missing)," missing ",
        if_one(id_missing, "value", "values"),
        " in '", var_names[i], "' ",
        "with ", deparse(replace)
      )
    }
    data[[i]][id_missing] <- replace
  }
  data
}
