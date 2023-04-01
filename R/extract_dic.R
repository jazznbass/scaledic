#' Extract a dictionary from a data file with dic information
#'
#' @param data A data frame with dic information.
#'
#' @return A data frame in a dictionary format.
#' @examples
#' extract_dic(ex_itrf)
#'
#' @export

extract_dic <- function(data) {

  id <- .get_dic_items(data, items_only = TRUE)

  dic_names <- lapply(data[id], function(x) names(attr(x, .opt$dic)))
  dic_names <- unlist(dic_names)
  dic_names <- unique(dic_names)
  #dic_names <- dic_names[which(!dic_names %in% c("var"))]

  N <- length(id)

  out <- matrix(NA, nrow = N, ncol = length(dic_names))
  out <- as.data.frame(out)
  names(out) <- dic_names

  for (row in 1:N) {
    dic <- attr(data[[id[row]]], opt("dic"))

    .var <- !dic_names %in% c("value_labels", "values", "missing")
    for (col in dic_names[.var]) {
      if (is.null(dic[[col]]) || length(dic[[col]]) == 0) {
        out[row, col] <- NA
      } else {
        out[row, col] <- dic[[col]]
      }
    }

    # values to code
    x <- dic[[opt("values")]]

    if (is.null(x)) {
      x <- NA
    } else if (!isTRUE(is.na(x))) {
      d <- diff(x)
      u <- unique(d)
      if (length(u) == 1 && u[1] == 1) {
        x <- paste0(min(x), ":", max(x))
      } else {
        x <- paste0(x, collapse = ",")
      }
    }

    out[row, opt("values")] <- x

    # value labels to code

    if (is.null(dic$value_labels)) {
      x <- NA
    } else {
      value_labels <- dic$value_labels[!is.na(dic$value_labels$value), ]
      x <- NA
      if (nrow(value_labels) > 0)
        x <- paste0(
          value_labels$value, " = ", value_labels$label, collapse = "; "
        )
    }

    out[row, opt("value_labels")] <- x

    # missing to code

    x <- dic[[.opt$missing]]
    if (is.null(x)) {
      x <- NA
    } else if (!isTRUE(is.na(x))) {
      x <- paste0(x, collapse = ", ")
      #x <- substring(x, 1, nchar(x) - 1)
    }
    out[row, opt("missing")] <- x

  }

  order <- c(.opt$item_name, .opt$item_label, .opt$values, .opt$value_labels,
             .opt$missing, .opt$weight)

  order <- order[order %in% names(out)]

  out <- out[, c(order, names(out)[which(!names(out) %in% order)])]

  if (!is.null(attributes(data)$dic$scales)) {
    new_rows <- attributes(data)$dic$scales
    new_ids <- (nrow(out) + 1):(nrow(out) + nrow(new_rows))
    out[new_ids, names(new_rows)] <- new_rows
  }

  out
}
