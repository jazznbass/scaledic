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
  "recodes" = "recodes",
  "numerics" = c("numeric", "integer", "double", "float"),
  dic_file_vars = c(
    "item_name",
    "item_label",
    "weight",
    "values",
    "value_labels",
    "missing",
    "type",
    "class",
    "score_filter",
    "score_function"
  )
)

#' Get function for element of opt list
#'
#' - takes a vector with elements
#' @keywords internal
opt <- function(x) {
  .opt[x] |> unlist() |> unname()
}

#' returns the index numbers of items in a data frame that have dic attributes
#'
#' @param data A data frame
#' @param items_only If TRUE, only items of class "item" are considered
#' @keywords internal
which_dic <- function(data, items_only = TRUE) {
  fn <- if (items_only) {
    function(x) !is.null(dic_attr(x)) && dic_attr(x, "class") == "item"
  } else {
    function(x) !is.null(dic_attr(x))
  }
  which(sapply(data, fn))
}

#' Decides based on suffix which function to use for reading a data.file
#'
#' @keywords internal
read_by_suffix <- function(filename) {
  ext <- tools::file_ext(filename)
  if (ext == "xlsx") return(readxl::read_xlsx(filename))
  if (ext == "xls") return(readxl::read_xls(filename))
  if (ext == "csv") return(utils::read.csv(filename))
  abort("File extension not recognised. ",
       "Please provide separately read data frame."
  )
}

#' Helper for singular or plural output
#'
#' @keywords internal
if_one <- function(x, singular, plural) {
  if (length(x) == 1) singular else plural
}

#' Returns the number of char occurrences in x
#'
#' @keywords internal
count_chars <- function(char, x) {
  sum(gregexpr(char, x)[[1]] > 0)
}

#' Check if a scalar or vector contains relevant infos
#'
#' @keywords internal
has_info <- function(x) {
  if (is.null(x) || isTRUE(is.na(x)) || identical(unname(x), "")) return(FALSE)
  TRUE
}

