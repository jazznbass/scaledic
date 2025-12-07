#' Replace Missing values
#'
#' @param data A data frame
#' @param replace Replace value for missing values
#' @param report If TRUE, proportion of missing values is printed.
#' @return A data frame with replaced missing values.
#' @export
replace_missing <- function(data, replace = NA, report = TRUE) {
  on.exit(print_messages())
  id <- which(sapply(data, function(x) !is.null(attr(x, .opt$dic))))
  var_names <- names(data)

  for (i in id) {
    id_missing <- which(data[[i]] %in% dic_attr(data[[i]], opt("missing")))
    if (report && length(id_missing)) {
      add_message(
        "Replaced ", length(id_missing)," missing ",
        if (length(id_missing == 1)) "value" else "values",
        " in '", var_names[i], "' ",
        #"(", round((length(id_missing) / sum(!is.na(data[[i]]))) * 100), "%)",
        "with ", deparse(replace)
      )
    }
    data[[i]][id_missing] <- replace
  }
  data
}
