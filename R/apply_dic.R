#' Apply dictionary to data frame
#'
#' Joins a data frame with a dictionary file.
#'
#' @param data Data frame or a character string with a filename (for now an
#'   Microsoft Excel file) containing the data.
#' @param dic A data frame comprising a dictionary or a character string with a
#'   filename (for now an Microsoft Excel file) containing a dictionary.
#' @param set_label_attr If TRUE, label attributes from the haven package are
#'   set. These labels are shown in the Rstudio View panel.
#' @param coerce_class If set TRUE mismatches between class and dic type are
#'   corrected.
#' @param replace_missing If TRUE, missing values from the dic are replaced with
#'   NAs in the data frame.
#' @param check_values If TRUE, performs the check_values function on the
#'   variables of the data frame included in the dic file.
#' @param impute_values If TRUE and score_scales is TRUE, missing values are
#'   automatically imputed based on scale information provided in the dic file.
#' @param score_scales If TRUE and the dic files contains score scale
#'   definitions these are applied to the data frame.
#' @param rename_var When a character is provided, corresponding column from the
#'   dic-file is used to rename variables from 'rename_var' (old) to 'item_name'
#'   (new) in the `data` data frame.
#' @param format_date Optional string that is applied when character variable is
#'   coerced to a 'Date' class.
#' @return A data frame with dictionary information.
#' @seealso \code{\link{new_dic}}, \code{\link{replace_missing}},
#'  \code{\link{score_from_dic}}, \code{\link{check_values
#'  }}, \code{\link{dic_haven}}
#' @author Jürgen Wilbert
#' @examples
#' dat <- apply_dic(dat_itrf, dic_itrf)
#' descriptives(dat)
#' @export
apply_dic <- function(data,
                      dic,
                      set_label_attr = TRUE,
                      coerce_class = TRUE,
                      replace_missing = TRUE,
                      score_scales = TRUE,
                      check_values = FALSE,
                      impute_values = FALSE,
                      rename_var = NULL,
                      format_date = "%Y-%m-%d") {

  init_messages(); on.exit(print_messages())

  if (inherits(dic, "character")) {
    add_message("Import ", dic, " as the dictionary file")
    dic <- read_by_suffix(dic)
  }
  if (inherits(data, "character")) {
    add_message("Import ", data, " as the data file")
    data <- read_by_suffix(data)
  }

  # check dic-file
  dic <- .clean_dic_file(dic)

  # rename variables by rename_var ---------------------------------------------
  if (!is.null(rename_var)) {
    data <- .rename_by_variable(rename_var, dic, data)
  }

  # extract scale scoring information from dic file -----

  .filter <- which(dic[[opt("class")]] == "scale")
  if (length(.filter) > 0) {
    dic_scores <- dic[.filter, , drop = FALSE]
    dic <- dic[-.filter, , drop = FALSE]
    add_message("Dictionary file includes scale definitions.")
    filter_col <- vapply(dic_scores, function(x) !all(is.na(x)), logical(1))
    filter_col <- sort(names(dic_scores)[filter_col])
    dic_scores <- dic_scores[, filter_col, drop = FALSE]
    dic_attr(data) <- list(scales = dic_scores)
  } else {
    dic_scores <- NULL
    score_scales <- FALSE
  }

  # identify valid variables from dic in data frame --------------------------
  id_match <- match(dic[[opt("item_name")]], names(data))
  keep <- !is.na(id_match)
  id_valid <- id_match[keep]
  i_dic_valid <- which(keep)

  var_not_df <- dic[[opt("item_name")]][which(is.na(id_match))]

  if (length(var_not_df) > 0) {
    add_message(
      length(var_not_df), " of ", nrow(dic),
      " items from dic not found in data file: ",
      paste0(var_not_df, collapse = ", ")
    )
  }
  id_valid <- id_valid[!is.na(id_valid)]

  # loop: apply dic information to each variable ------------------------------

  for (i in seq_along(id_valid)) {
    i_dic  <- i_dic_valid[i]   # dic row index
    args <- c(
      list(x = data[[id_valid[i]]]),
      as.list(dic[i_dic, ]),
      list(.coerce_class = coerce_class, .format_date = format_date)
    )
    data[[id_valid[i]]] <- do.call("new_dic", args)
  }

  if (score_scales) {
    replace_missing <- TRUE
    check_values <- TRUE
  }

  # replace missing ----
  if (replace_missing) {
    data <- replace_missing(data)
  }

  # check values ----
  if (check_values) {
    data <- check_values(data, replace = NA)
  }

  # score scales ----
  if (score_scales) {
    data <- score_from_dic(data, impute_values = impute_values)
  }

  # return ----

  if (set_label_attr) {
    data <- dic_haven(data)
  }
  data
}

.clean_dic_file <- function(dic) {

  # rename old dic names
  names(dic) <- tolower(names(dic))
  names(dic)[which(names(dic) %in% c("label", "name"))] <- opt("item_name")
  names(dic)[which(names(dic) %in% "item")] <- opt("item_label")
  names(dic)[which(names(dic) %in% "sub_scale_2")] <- "subscale_2"
  names(dic)[which(names(dic) %in% "sub_scale")] <- "subscale"
  names(dic)[which(names(dic) %in% "sub_scale_label")] <- "subscale_label"
  names(dic)[which(names(dic) %in% "sub_scale_2_label")] <- "subscale_2_label"

  # delete empty rows
  dic <- dic[apply(dic, 1, function(x) !all(is.na(x))), , drop = FALSE]

  # delete rows with comments (first sign of the first column is a #)
  .filter <- which(apply(dic, 1, function(x) isTRUE(substr(as.character(x[1]), 1, 1) == "#")))
  if (length(.filter) > 0) dic <- dic[-.filter, , drop = FALSE]

  # filter if variable "active" is available
  if ("active" %in% names(dic)) {
    dic <- dic[which(dic$active %in% c(1, "1", TRUE)), , drop = FALSE]
  }

  # remove white spaces
  if (opt("value_labels") %in% names(dic)) {
    dic[[opt("value_labels")]] <- trimws(dic[[opt("value_labels")]])
    dic[[opt("value_labels")]][which(dic[[opt("value_labels")]] == "")] <- NA
  }
  dic[[opt("item_name")]] <- trimws(dic[[opt("item_name")]])

  # check for missing class variable
  if (is.null(dic[[opt("class")]])) dic[[opt("class")]] <- "item"

  # set class to scale if score_filter is defined
  if (opt("score_filter") %in% names(dic)) {
    .filter <- which(!is.na(dic[[opt("score_filter")]]))
    dic[.filter, opt("class")] <- "scale"
  }

  # check for missing type variable
  if (is.null(dic[[opt("type")]])) {
    dic[[opt("type")]] <- NA
  }

  # missing values and value_labels
  if (is.null(dic[[opt("values")]])) dic[[opt("values")]] <- NA
  if (is.null(dic[[opt("value_labels")]])) dic[[opt("value_labels")]] <- NA

  # check for duplicated item_names in dic
  .duplicates <- duplicated(dic[[opt("item_name")]])
  if (any(.duplicates)) {
    id <- dic[[opt("item_name")]][which(.duplicates)]
    stop(
      paste0("Item names duplicated in dic-file: ", paste0(id, collapse = ", "))
    )
  }

  dic
}

.rename_by_variable <- function(rename_var, dic, data) {
  if (!rename_var %in% names(dic)) {
    add_message("Rename variable '", rename_var, "' not found in dic-file.", frame = -2)
  } else {
    to_from <- setNames(dic[[rename_var]], dic[[opt("item_name")]])
    to_from <- to_from[!is.na(to_from)]
    .duplicates <- names(to_from) %in% names(data)
    if(any(.duplicates)){
      add_message("Skipped renaming column to an already existing name: ",
                  paste0(names(to_from)[.duplicates], collapse = ", "), frame = -2)
      to_from <- to_from[!.duplicates]

    }
    add_message(
      "Took the '", rename_var, "' column to rename ",
      sum(!.duplicates), " item names.", frame = -2
    )
    nm <- names(data)
    m  <- match(nm, unname(to_from), nomatch = 0L)
    nm[m > 0L] <- names(to_from)[m[m > 0L]]
    names(data) <- nm
  }
  data
}
