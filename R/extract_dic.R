#' Extract a dictionary from data file
#'
#' @param data A data frame
#'
#' @return A data frame in a dictionary format
#' @export

extract_dic <- function(data) {

  id <- scaledic:::.get_dic_items(data, items_only = FALSE)

  dic_names <- lapply(data[id], function(x) names(attr(x, scaledic:::.opt$dic)))
  dic_names <- unlist(dic_names)
  dic_names <- unique(dic_names)
  #dic_names <- dic_names[which(!dic_names %in% c("var"))]

  N <- length(id)

  out <- matrix(NA, nrow = N, ncol = length(dic_names))
  out <- as.data.frame(out)
  names(out) <- dic_names

  for (row in 1:N) {
    dic <- attr(data[[id[row]]], .opt$dic)

    for (col in dic_names[!dic_names %in% c("value_labels", "values", "missing")]) {
      #print(col)
      if (is.null(dic[[col]]) || length(dic[[col]]) == 0) {
        out[row, col] <- NA
      } else {
        out[row, col] <- dic[[col]]
      }
    }

    # values to code
    x <- dic[[.opt$values]]
    d <- diff(x)
    u <- unique(d)
    if (length(u) == 1 && u[1] == 1) {
      x <- paste0(min(x), ":", max(x))
    } else {
      x <- paste0(x, collapse = ",")
    }

    #x <- paste0(dic[[.dic_file$values]], collapse = ",")
    #x <- substring(x, 1, nchar(x) - 1)
    out[row, .opt[["values"]]] <- x

    # value labels to code

    x <- paste0(dic$value_labels$value, " = ", dic$value_labels$label, collapse = "; ")
    #x <- paste0(dic$values, " = ", names(dic$values), collapse = "; ")
    out[row, .opt[["value_labels"]]] <- x

    x <- paste0(dic[[.opt$missing]], ",", collapse = "")
    x <- substring(x, 1, nchar(x) - 1)
    out[row, .opt[["missing"]]] <- x
  }

  order <- c(.opt$item_name, .opt$item_label, .opt$values, .opt$value_labels, .opt$missing, .opt$weight)
  out <- out[, c(order, names(out)[which(!names(out) %in% order)])]
  #out <- out[order(out[[.opt$item_name]]),]
  out
}
