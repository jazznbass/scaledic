# option parameters

.opt <- list(
  "dic" = "dic",
  "item_name" = "item_name",
  "item_label" = "item_label",
  "weight" = "weight",
  "type" = "type",
  "values" = "values",
  "value_labels" = "value_labels",
  "missing" = "missing",
  "class" = "class",
  "score_filter" = "score_filter",
  "score_function" = "score_function",
  "numerics" = c("numeric", "integer", "double", "float")
)

opt <- function(x) {
  .opt[[x]]
}


# Names of the dic file variables. Order determines order when using extract_dic
.dic_file <- list(
  "item_name" = "item_name",
  "item_label" = "item_label",
  "weight" = "weight",
  "values" = "values",
  "value_labels" = "value_labels",
  "missing" = "missing",
  "type" = "type",
  "class" = "class",
  "score_filter" = "score_filter",
  "score_function" = "score_function"
)


.get_dic_items <- function(data, items_only = TRUE) {

  fn <- if (items_only) {
    function(x) !is.null(attr(x, opt("dic"))) && dic_attr(x, "class") == "item"
  } else {
    function(x) !is.null(attr(x, opt("dic")))
  }

  which(sapply(data, fn))

}

.get_index <- function(data, filter, names = TRUE, class = NULL) {
  if (!is.null(class)) filter <- paste0(filter, " & class == '", class, "'")
  dic <- extract_dic(data)
  id <- with(dic, eval(str2lang(filter)))
  out <- which(names(data) %in% dic[[.opt$item_name]][id])
  if (names) out <- names(data)[out]
  out
}

.to_filter <- function(scale = NULL, subscale = NULL, subscale_2 = NULL) {

  filter <- NULL

  if (!is.null(scale))
    filter <- paste0(
      "scale %in% c(", paste0("'", scale,"'", collapse = ", "), ")",
      collapse = ""
    )

  if (!is.null(subscale)) {
    tmp <- paste0(
      "subscale %in% c(", paste0("'", subscale,"'", collapse = ", "), ")",
      collapse = ""
    )
    if (is.null(filter)) {
      filter <- tmp
    } else {
      filter <- paste0(filter, " & ", tmp)
    }
  }

  if (!is.null(subscale_2)) {
    tmp <- paste0(
      "subscale_2 %in% c(", paste0("'", subscale_2,"'", collapse = ", "), ")",
      collapse = ""
    )
    if (is.null(filter)) {
      filter <- tmp
    } else {
      filter <- paste0(filter, " & ", tmp)
    }
  }

  filter

}

.mean <- function(x, min_valid, max_na) {

  if (isTRUE(min_valid < 1) && isTRUE(min_valid > 0)) min_valid <- trunc(min_valid * length(x))
  if(isTRUE(sum(!is.na(x)) < min_valid)) {
    return(NA)
  }

  if (isTRUE(max_na < 1) && isTRUE(max_na > 0))
    max_na <- trunc(max_na * length(x))
  if(isTRUE(sum(is.na(x)) > max_na)) {
    return(NA)
  }

  mean(x, na.rm = TRUE)

}

.weighted_mean <- function(x, weights) {
  weighted.mean(x, weights, na.rm = TRUE)
}

.weighted_sum <- function(x, weights) {
  sum(x * weights, na.rm = TRUE)
}

.nice_num <- function(x, digits = 2) {
  fmt <- paste0("%.", digits, "f")
  sub("^(-?)0.", "\\1.", sprintf(fmt, x))
}

.read_by_suffix <- function(filename, sheet = NULL) {
  ext <- tools::file_ext(filename)
  if (ext == "xlsx") return(readxl::read_xlsx(filename))
  if (ext == "xls") return(readxl::read_xls(filename))
  if (ext == "csv") return(utils::read.csv(filename))
  stop("File extension not recognised. ",
       "Please provide separately read data frame."
  )
}

return_messages <- function(msg, warning = FALSE) {
  if (length(msg) == 0) return(FALSE)
  msg <- table(msg)
  for(i in seq_along(msg)){
    if (msg[i] > 1) names(msg)[i] <- paste0(names(msg)[i], " (", msg[i], "x)")
  }
  msg <- paste0(1:length(msg), ": ", names(msg), collapse = "\n")
  msg <- paste0("\n", msg, "\n")
  if (warning) warning(msg, call. = FALSE) else message(msg)
}
