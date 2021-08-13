# option parameters

.opt <- list(
  "dic" = "dic",
  "item_name" = "item_name",
  "item_label" = "item_label",
  "scale" = "scale",
  "subscale" = "subscale",
  "subscale_2" = "subscale_2",
  "scale_label" = "scale_label",
  "subscale_label" = "subscale_label",
  "subscale_2_label" = "subscale_2_label",
  #"index" = "index",
  "weight" = "weight",
  "source" = "source",
  #"note" = "note",
  "type" = "type",
  "values" = "values",
  "value_labels" = "value_labels",
  "missing" = "missing",
  "class" = "class",
  "score_filter" = "score_filter",
  "score_function" = "score_function"
)

# Names of the dic file variables. Order determines order when using extract_dic
.dic_file <- list(
  "item_name" = "item_name",
  "scale" = "scale",
  "subscale" = "subscale",
  "subscale_2" = "subscale_2",
  #"index" = "index",
  "scale_label" = "scale_label",
  "subscale_label" = "subscale_label",
  "subscale_2_label" = "subscale_2_label",
  "item_label" = "item_label",
  "weight" = "weight",
  "values" = "values",
  "value_labels" = "value_labels",
  "missing" = "missing",
  "type" = "type",
  "source" = "source",
  "class" = "class",
  "score_filter" = "score_filter",
  "score_function" = "score_function"
  #"note" = "note"
)


.get_dic_items <- function(data, items_only = TRUE) {

  if (!items_only)
    foobar <- function(x) !is.null(attr(x, .opt$dic))

  if (items_only)
    foobar <- function(x) !is.null(attr(x, .opt$dic)) && dic_attr(x, .opt$class) == "item"
  which(sapply(data, foobar))

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
    filter <- paste0("scale %in% c(", paste0("'", scale,"'", collapse = ", "), ")", collapse = "")

  if (!is.null(subscale)) {
    tmp <- paste0("subscale %in% c(", paste0("'", subscale,"'", collapse = ", "), ")", collapse = "")
    if (is.null(filter)) {
      filter <- tmp
    } else {
      filter <- paste0(filter, " & ", tmp)
    }
  }

  if (!is.null(subscale_2)) {
    tmp <- paste0("subscale_2 %in% c(", paste0("'", subscale_2,"'", collapse = ", "), ")", collapse = "")
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

  if (isTRUE(max_na < 1) && isTRUE(max_na > 0)) max_na <- trunc(max_na * length(x))
  if(isTRUE(sum(is.na(x)) > max_na)) {
    return(NA)
  }

  mean(x, na.rm = TRUE)

}

.sum <- function(x, min_valid, max_na) {

  if (isTRUE(min_valid < 1) && isTRUE(min_valid > 0)) min_valid <- trunc(min_valid * length(x))
  if(isTRUE(sum(!is.na(x)) < min_valid)) {
    return(NA)
  }

  if (isTRUE(max_na < 1) && isTRUE(max_na > 0)) max_na <- trunc(max_na * length(x))
  if(isTRUE(sum(is.na(x)) > max_na)) {
    return(NA)
  }

  sum(x, na.rm = TRUE)

}

