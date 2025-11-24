#' Apply dictionary
#'
#' Joins a data frame with a dictionary file.
#'
#' @param data Data frame
#' @param dic A data frame comprising a dictionary or a character string with a
#'   filename (for now an Microsoft Excel file) containing a dictionary.
#' @param factors If set TRUE, variables defined as type `factor` in the dic
#'   file will be turned into factors.
#' @param set_label_attr If TRUE, label attributes from the haven package are
#'   set. These labels are shown in the Rstudio View panel.
#' @param coerce_class If set TRUE mismatches between class and dic type are
#'   corrected.
#' @param replace_missing If TRUE, missing values from the dic are replaced with
#'   NA
#' @param check_values If TRUE, performs the check_values function on the
#'   variables of the data frame included in the dic file.
#' @param impute_values If TRUE and score_scales is TRUE, missing values are
#'   automatically imputed based on scale information provided in the dic file.
#' @param score_scales If TRUE and the dic files contains score scale
#'   definitions these are applied
#' @param rename_var When a character is provided, corresponding column from the
#'   dic file is used to rename variables from rename_var to item_name.
#' @return A data frame with dictionary information.
#' @examples
#' dat <- apply_dic(dat_itrf, dic_itrf)
#' descriptives(dat)
#' @export
apply_dic <- function(data,
                      dic,
                      factors = TRUE,
                      set_label_attr = TRUE,
                      coerce_class = TRUE,
                      replace_missing = TRUE,
                      score_scales = TRUE,
                      check_values = FALSE,
                      impute_values = FALSE,
                      rename_var = NULL) {

  msg <- c()

  if (inherits(dic, "character")) dic <- .read_by_suffix(dic)
  if (inherits(data, "character")) data <- .read_by_suffix(data)

  # missing missing variable
  if (is.null(dic[[opt("missing")]])) replace_missing <- FALSE

  dic <- .clean_dic_file(dic, msg)
  msg <- attr(dic, "msg")

  # rename variables by rename_var ---------------------------------------------

  if (!is.null(rename_var)) {
    to_from <- setNames(dic[[rename_var]], dic[[opt("item_name")]])
    to_from <- to_from[!is.na(to_from)]
    .duplicates <- names(to_from) %in% names(data)
    if(any(.duplicates)){
      msg <- c(msg, paste0(
        "Skipped renaming column to an already existing name: ",
        paste0(names(to_from)[.duplicates], collapse = ", ")
      ))
      to_from <- to_from[!.duplicates]
    }
    for(i in 1:length(to_from)) {
      names(data)[which(names(data) == to_from[i])] <- names(to_from[i])
    }
  }

  # extract scoring information from dic file -----

  .filter <- dic[[opt("class")]] == "scale"
  dic_scores <- dic[.filter, ]
  dic <- dic[!.filter, ]

  if (nrow(dic_scores) > 0) {
    filter <- unlist(lapply(dic_scores, function(x) !all(is.na(x))))
    filter <- sort(names(dic_scores)[filter])
    dic_scores <- dic_scores[, filter]
    dic_attr(data) <- list(scales = dic_scores)
  }

  # loop: apply dic information to each variable --------------------------------------

  var_not_df <- NULL

  for (i in 1:nrow(dic)) {

    # look up column in data frame corresponding to item_name
    id <- which(names(data) == dic[[opt("item_name")]][i])
    if (length(id) > 1) {
      msg <- c(msg, paste0(
        "Multiple ids found",
        paste0(names(data)[id], collapse = ", ")
      ))
    }

    if (length(id) == 0) {
      var_not_df <- c(var_not_df, dic[[opt("item_name")]][i])
      next
    }

    dic_row <- as.list(dic[i, ])

    if (has_info(dic_row$values)) {
      dic_row$values <- paste0(
        "c(", as.character(dic_row$values), ")")  |>
        str2lang()  |>
        eval()
    }

    if (has_info(dic_row$missing)) {
      dic_row$missing <- paste0(
        "c(", as.character(dic_row$missing), ")")  |>
        str2lang()  |>
        eval()
    }

    # set dic attributes for further variables
    .id <- which(!names(dic) %in% c(
      "item_name", "item_label", "values", "value_labels", "missing", "weight",
      "type", "class"
    ))
    further_attributes <- names(dic)[.id]

    data[[id]] <- new_dic(
      data[[id]],
      item_name = dic_row$item_name,
      item_label = dic_row$item_label,
      values = dic_row$values,
      value_labels = dic_row$value_labels,
      missing = dic_row$missing,
      weight = dic_row$weight,
      type = dic_row$type,
      recodes = dic_row$recodes,
      class = dic_row$class,
      .list = dic_row[further_attributes],
      .coerce_class = coerce_class,
      .message_attr = TRUE
    )

    msg <- c(msg, attr(data[[id]], "messages"))
    attr(data[[id]], "messages") <- NULL
  }

  # var not found messages ------

  if (!is.null(var_not_df)) {
    msg <- c(msg, paste0(
      length(var_not_df), " of ", nrow(dic),
      " variables from dic not found in data file:",
      paste0(var_not_df, collapse = ", ")
    ))
  }

  # replace missing ----
  if (replace_missing) {
    data <- replace_missing(data)
    msg <- c(msg, "Missing values replaced with NA")
  }

  # check values ----
  if (check_values) {
    msg <- c(msg, "Invalid values replaced with NA")
    vars <- names(data) %in% dic[[.opt$item_name]]
    data[, vars] <- check_values(data[, vars], replace = NA)
  }

  # score scales ----
  if (score_scales && nrow(dic_scores > 0)) {
    msg <- c(msg, "Invalid values replaced with NA")
    vars <- names(data) %in% dic[[.opt$item_name]]
    data[, vars] <- check_values(data[, vars], replace = NA)
    if (impute_values) msg <- c(msg, "Scales imputed")
    msg <- c(msg, "Scales scored")
    data <- score_from_dic(data, impute_values = impute_values)
  }

  # return ----

  if (set_label_attr) {
    data <- dic_haven(data)
  }

  return_messages(msg)

  data
}

.clean_dic_file <- function(dic, msg) {

  #rename dic names
  names(dic) <- tolower(names(dic))
  names(dic)[which(names(dic) %in% c("label", "name"))] <- "item_name"
  names(dic)[which(names(dic) %in% "item")] <- "item_label"
  names(dic)[which(names(dic) %in% "sub_scale_2")] <- "subscale_2"
  names(dic)[which(names(dic) %in% "sub_scale")] <- "subscale"
  names(dic)[which(names(dic) %in% "sub_scale_label")] <- "subscale_label"
  names(dic)[which(names(dic) %in% "sub_scale_2_label")] <- "subscale_2_label"

  # delete empty rows
  dic <- dic[apply(dic, 1, function(x) !all(is.na(x))),]

  # delete rows with comments (first sign of the first column is a #)
  .filter <- which(apply(dic, 1, function(x) substr(x[1], 1, 1) == "#"))
  if (length(.filter) > 0) dic <- dic[-.filter, ]

  # filter if variable "active" is available --------------------------------

  if ("active" %in% names(dic)) {
    dic <- dic[which(dic$active == 1), ]
  }

  #remove white spaces

  .column <- .dic_file$value_labels

  if (!is.null(dic[[.column]])) {
    dic[[.column]] <- trimws(dic[[.column]])
    dic[[.column]][which(dic[[.column]] == "")] <- NA
  }
  dic[[.dic_file$item_name]] <- trimws(dic[[.dic_file$item_name]])

  # check for missing class variable
  if (is.null(dic[[.dic_file$class]])) dic[[.dic_file$class]] <- "item"

  if (.dic_file$score_filter %in% names(dic)) {
    .filter <- which(!is.na(dic[[.dic_file$score_filter]]))
    dic[.filter, .dic_file$class] <- "scale"
  }

  filter_items <- dic[[.dic_file$class]] == "item"

  # check for missing type variable
  if (is.null(dic[[.dic_file$type]])) {
    dic[[.dic_file$type]] <- NA
  }

  # type NA to integer
  miss_type <- which(is.na(dic[[.dic_file$type]]))
  if (length(miss_type) > 0) {
    msg <- c(msg, paste0(
      "'type' attribute missing and replaced with an estimation (", length(miss_type), "x)"
    ))
  }

  # missing values and value_labels
  if (is.null(dic[[.dic_file$values]])) dic[[.dic_file$values]] <- NA
  if (is.null(dic[[.dic_file$value_labels]])) dic[[.dic_file$value_labels]] <- NA

  # weight NA to 1
  miss_weight <- which(is.na(dic[[.dic_file$weight]]) & filter_items)
  if (length(miss_weight) > 0) {
    msg <- c(msg, paste0(
      "Missing weight found and replaced with 1 (", length(miss_weight), "x)"
    ))
    dic[miss_weight, .dic_file$weight] <- 1
  }

  # set report missing label
  missing_label <- NULL
  for (i in 1:nrow(dic)){
    if (is.na(dic[[.dic_file$item_label]][i])) missing_label <- c(missing_label, i)
  }
  if (!is.null(missing_label)) {
    msg <- c(msg, paste0(
      "Missing item_label found at variabel no. ",
      paste0(missing_label, collapse = ", "), " in dic file."
    ))
  }

  # check for duplicated item_names in dic
  .duplicates <- duplicated(dic[[.dic_file$item_name]])
  if (any(.duplicates)) {
    id <- dic[[.dic_file$item_name]][which(.duplicates)]
    stop(
      paste0("Item names duplicated in dic-file: ", paste0(id, collapse = ", "))
    )
  }

  # Check for \r\n in value labels

  if (any(grepl("\r\n", x = dic[[.dic_file$value_labels]]))) {
    msg <- c(msg, "Found linebreaks in value_labels and replaced them with ';'")
    dic[[.dic_file$value_labels]] <- gsub("\r\n", "; ", x =  dic[[.dic_file$value_labels]])
  }

  attr(dic, "msg") <- msg
  dic
}
