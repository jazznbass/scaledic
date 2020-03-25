#' Extract a dictionary from data file
#'
#' @param data A data frame
#'
#' @return A data frame in a dictionary format
#' @export

extract_dic <- function(data) {

  id <- .get_dic_items(data)
  dic_names <- lapply(data[id], function(x) names(attr(x, .opt$dic)))
  dic_names <- unlist(dic_names)
  dic_names <- unique(dic_names)
  dic_names <- dic_names[which(!dic_names %in% c("var"))]

  N <- length(id)

  out <- matrix(NA, nrow = N, ncol = length(dic_names))
  out <- as.data.frame(out)
  names(out) <- dic_names

  for (row in 1:N) {
    dic <- attr(data[[id[row]]], .opt$dic)

    for (col in dic_names[!dic_names %in% c("values", "missing")]) {
      if (is.null(dic[[col]])) {
        out[row, col] <- NA
      } else {
        out[row, col] <- dic[[col]]
      }
    }

    x <- paste0(dic[[.dic_file$values]], ",", collapse = "")
    x <- substring(x, 1, nchar(x) - 1)
    out[row, .dic_file[["values"]]] <- x
    x <- paste0(dic$values, " = ", names(dic$values), "; ", collapse = "")
    x <- substring(x, 1, nchar(x) - 2)
    out[row, .dic_file[["value_labels"]]] <- x
    x <- paste0(dic[[.dic_file$missing]], ",", collapse = "")
    x <- substring(x, 1, nchar(x) - 1)
    out[row, .dic_file[["missing"]]] <- x
  }

  order <- c(.opt$item_name, .opt$item_label, .opt$values, .opt$value_labels, .opt$missing, .opt$weight)
  out <- out[, c(order, names(out)[which(!names(out) %in% order)])]
  #out <- out[order(out[[.opt$item_name]]),]
  out
}

extract_dic_old <- function(data) {

  opt.attr <- .dic_file[
    !names(.dic_file) %in% c("variable", "values", "value_labels")
    ]
  opt.attr <- c(opt.attr, list(class = "class"))
  dic_names <- names(opt.attr)
  id <- .get_dic_items(data)
  N <- length(id)
  vars <- c(unlist(.dic_file[!names(.dic_file) %in% "variable"]), class = "class")
  out <- matrix(NA, nrow = N, ncol = length(vars))
  out <- as.data.frame(out)
  names(out) <- vars

  for (i in 1:N) {
    dic <- attr(data[[id[i]]], .opt$dic)
    for (j in 1:length(opt.attr)) {
      out[i, opt.attr[[j]]] <- dic[[dic_names[j]]]
    }
    x <- paste0(dic$values, ",", collapse = "")
    x <- substring(x, 1, nchar(x) - 1)
    out[i, .dic_file[["values"]]] <- x
    x <- paste0(dic$values, " = ", names(dic$values), "; ", collapse = "")
    x <- substring(x, 1, nchar(x) - 2)
    out[i, .dic_file[["value_labels"]]] <- x
  }



  out
}
