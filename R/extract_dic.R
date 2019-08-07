#' Extract a dictionary from data file
#'
#' @param data A data frame
#'
#' @return A data frame in a dictionary format
#' @export

extract_dic <- function(data) {
  opt.attr <- list(
    "item_label_short" = "LABEL",
    "scale" = "SCALE",
    "subscale" = "SUB_SCALE",
    "subscale_2" = "SUB_SCALE_2",
    "scale_label" = "SCALE_LABEL",
    "subscale_label" = "SUB_SCALE_LABEL",
    "subscale_2_label" = "SUB_SCALE_2_LABEL",
    "index" = "INDEX",
    "item_label" = "ITEM",
    "weight" = "WEIGHT",
    "source" = "SOURCE",
    "note" = "NOTE",
    "type" = "TYPE",
    # "values" = "VALUES",
    # "value_labels" = "VALUE_LABELS",
    "missing" = "MISSING"
  )

  dic_names <- names(opt.attr)

  #id <- which(sapply(data, function(x) !is.null(attr(x, .opt$dic))))
  id <- .get_dic_items(data)
  N <- length(id)

  vars <- list(
    "LABEL", "SCALE", "SCALE_LABEL", "SUB_SCALE", "SUB_SCALE_LABEL",
    "SUB_SCALE_2", "SUB_SCALE_2_LABEL",
    "INDEX", "ITEM", "WEIGHT", "VALUES", "VALUE_LABELS",
    "MISSING", "TYPE", "SOURCE", "NOTE"
  )

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
    out[i, "VALUES"] <- x
    x <- paste0(dic$values, " = ", names(dic$values), "; ", collapse = "")
    x <- substring(x, 1, nchar(x) - 2)
    out[i, "VALUE_LABELS"] <- x
  }

  out
}
