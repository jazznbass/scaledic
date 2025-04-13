#' Print dic infos
#'
#' @param x A variable with dic infos
#'
#' @return Dic infos of x
#' @keywords internal
#' @export
dic <- function(data) {
  prefix <- "# "
  first_line <- paste0(prefix, dic_attr(data, "item_label"))
  if (!is.null(dic_attr(data, "scale_label")) && !identical(dic_attr(data, "scale_label"), NA))
    first_line <- paste0(
      first_line, " (", dic_attr(data, "scale_label"), ")",
      collapse = ""
    )

  if (dic_attr(data, "class") == "score") {
    first_line <- paste0(
      dic_attr(data, "item_label"), "\n(",
      dic_attr(data, "score_function"), " of items: ",
      dic_attr(data, "score_filter"), ")",
      collapse = ""
    )
  }

  if (length(first_line) > 0) cat(first_line, "\n")

  data_type <- dic_attr(data, "type")
  cat(prefix, "Data type is ", data_type, "\n", sep = "")


  values <- dic_attr(data, "values")

  if (has_info(values)) {

    if (data_type == "integer") {
      d <- diff(values)
      u <- unique(d)
      if (length(u) == 1 && u[1] == 1) {
        x <- paste0(min(values), ":", max(values))
      } else {
        x <- paste0(values, collapse = ",")
      }
      cat(prefix, "Valid values: ", x, "\n", sep = "")
    }
    if (data_type %in% c("float", "numeric")) {
      cat(
        prefix, "Valid values: ", "From ", min(values), " to ", max(values),
        "\n", sep = ""
      )
    }

    if (!is.null(names(values))) {
      id <- which(!is.na(names(values)) & values != "")
      cat(prefix, "Value labels:\n", sep = "")
      cat(paste0(prefix, "  ",values[id], " = ", names(values[id]), collapse = "\n"))
    }

  }

  class(data) <- class(data)[!class(data) %in% opt("dic")]
  dic_attr(data) <- NULL
  attr(data, "label") <- NULL
  attr(data, "labels") <- NULL
  if (length(first_line) > 0) cat("\n")


  ldat <- length(data)
  max <- ifelse(ldat < 100, ldat, 100)
  cat(prefix, "Length: ", ldat, "\n", sep = "")

  print(data[1:max])
  if (ldat > 100) {
    cat(prefix, "entries omitted\n", sep = "")
  }

  invisible(dic_attr(data))
}
