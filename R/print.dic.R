#' Print dic infos
#'
#' @param x A variable with dic infos
#' @param ... Further parameters for the print function
#'
#' @return Dic infos of x
#' @export
print.dic <- function(x, ...) {
  print_dic(x, ...)
}

#' Prints dic infos of a variable with dic infos.
#'
#' @param x A variable with dic infos.
#' @param length Number of entries to be printed.
#' @param ... Unused future parameters.
#'
#' @return Dic infos of x and the first `length` entries of `x`.
#' @export
print_dic <- function(x, length = 100, ...) {

  dicinfo <- dic_attr(x)
  prefix <- getOption("scaledic.string.prefix")

  first_line <- paste0(prefix, dic_attr(x, "item_label"))
  scale_label <- dic_attr(x, "scale_label")
  if (has_info(scale_label))
    first_line <- paste0(
      first_line, " (", scale_label, ")",
      collapse = ""
    )

  if (dic_attr(x, "class") == "score") {
    first_line <- paste0(
      dic_attr(x, "item_label"), "\n(",
      dic_attr(x, "score_function"), " of items: ",
      dic_attr(x, "score_filter"), ")",
      collapse = ""
    )
  }

  if (length(first_line) > 0) cat(first_line, "\n")

  x_type <- dic_attr(x, "type")
  cat(prefix, "Data type is ", x_type, "\n", sep = "")

  values <- dic_attr(x, "values")

  if (has_info(values)) {

    if (x_type == "integer") {
      d <- diff(values)
      u <- unique(d)
      values_str <- if (length(u) == 1 && u[1] == 1) {
        paste0(min(values), ":", max(values))
      } else {
        paste0(values, collapse = ",")
      }
      cat(prefix, "Valid values: ", values_str, "\n", sep = "")
    }

    if (x_type %in% c("double", "float", "numeric")) {
      cat(
        prefix, "Valid values: ", "From ", min(values), " to ", max(values),
        "\n", sep = ""
      )
    }

    if (!is.null(names(values))) {
      id <- which(!is.na(names(values)) & nzchar(names(values)))
      cat(prefix, "Value labels:\n", sep = "")
      cat(paste0(prefix, "  ",values[id], " = ", names(values[id]), collapse = "\n"))
      cat("\n")
    }

    if (!is.null(dic_attr(x, "recodes"))) {
      recodes <- dic_attr(x, "recodes")
      cat(prefix, "Recodes:\n", sep = "")
      cat(paste0(
        prefix, "  ", recodes[[1]], " = ",
        recodes[[2]], collapse = "\n"
      ))
    }

  }

  x_plain <- x
  class(x_plain) <- setdiff(class(x_plain), "dic")
  dic_attr(x_plain) <- NULL
  attr(x_plain, "label") <- NULL
  attr(x_plain, "labels") <- NULL

  if (length(first_line) > 0) cat(prefix, "\n")

  ldat <- length(x_plain)
  max <- ifelse(ldat < length, ldat, length)

  cat(
    prefix, "Length is ", ldat,
    " (", sum(is.na(x_plain), na.rm = TRUE), " NA",
    "; ", length(id_invalid_values(x)), " invalid",
    ")\n",
    sep = ""
  )

  .print_with_prefix(x_plain[1:max])
  if (ldat > length) {
    cat(prefix, "entries omitted\n", sep = "")
  }

  invisible(dicinfo)
}


.print_with_prefix <- function(x, ...) {
  output <- capture.output(print(x, ...))
  cat(paste0(getOption("scaledic.string.prefix"), output), sep = "\n")
  invisible(x)
}

