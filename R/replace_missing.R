#' Replace Missing values
#'
#' @param data A data frame
#' @param replace Replace value for missing values
#' @param report If TRUE, proportion of missing values is printed.
#' @return A data frame with replaced missing values.
#' @export
replace_missing <- function(data, replace = NA, report = FALSE) {
  id <- which(sapply(data, function(x) !is.null(attr(x, .opt$dic))))
  var_names <- names(data)

  for (i in id) {
    missing <- dic_attr(data[[i]], .opt$missing)
    id_missing <- which(data[[i]] %in% missing)
    if (report) {
      cat(
        var_names[i], ": ",
        round((length(id_missing) / sum(!is.na(data[[i]]))) * 100),
        "% (", length(id_missing), ")\n",
        sep = ""
      )
    }
    data[[i]][id_missing] <- replace
  }
  data
}
