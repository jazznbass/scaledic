old_apply_dic <- function(data,
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
  if (is.null(dic[[.dic_file$missing]])) replace_missing <- FALSE

  dic <- .clean_dic_file(dic, msg)
  msg <- attr(dic, "msg")

  # rename variables by rename_var ---------------------------------------------

  if (!is.null(rename_var)) {
    to_from <- setNames(dic[[rename_var]], dic[[.dic_file$item_name]])
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

  .filter <- dic[[.dic_file$class]] == "scale"
  dic_scores <- dic[.filter, ]
  dic <- dic[!.filter, ]

  if (nrow(dic_scores) > 0) {
    filter <- unlist(lapply(dic_scores, function(x) !all(is.na(x))))
    filter <- sort(names(dic_scores)[filter])
    dic_scores <- dic_scores[, filter]
    attr(data, opt("dic")) <- list(scales = dic_scores)
  }

  # loop: apply dic information to each variable --------------------------------------

  var_not_df <- NULL

  for (i in 1:nrow(dic)) {
    # look up column in data frame corresponding to item_name
    id <- which(names(data) == dic[[.dic_file$item_name]][i])
    if (length(id) > 1) {
      msg <- c(msg, paste0(
        "Multiple ids found",
        paste0(names(data)[id], collapse = ", ")
      ))
    }

    if (length(id) == 0) {
      var_not_df <- c(var_not_df, dic[[.dic_file$item_name]][i])
      next
    }

    # type (class)
    if (is.na(dic[i, .dic_file$type])) {
      dic[i, .dic_file$type] <- "integer" # default estimation
      if (inherits(data[[id]], "numeric")) dic[i, .dic_file$type] <- "float"
      if (inherits(data[[id]], "character")) dic[i, .dic_file$type] <- "character"
      if (inherits(data[[id]], "factor")) dic[i, .dic_file$type] <- "factor"
    }

    # numeric:
    if (dic[i, .dic_file$type] %in% c("integer","numeric","float","double")) {
      if (!inherits(data[[id]], c("integer","numeric","double"))) {

        if (coerce_class) {
          msg <- c(msg, paste0(
            "Class should be numeric but is ", class(data[[id]]),
            ". Corrected to numeric: ", names(data)[id]
          ))
          class(data[[id]]) <- "numeric"
        } else {
          msg <- c(msg, paste0(
            "Class should be numeric but is ", class(data[[id]]),
            ": ",names(data)[id]
          ))
        }
      }
    }


    # extract values
    if (!is.na(dic[i, .dic_file$values]) && dic[i, .dic_file$values] != "") {
      values <- paste0("c(", as.character(dic[i, .dic_file$values]), ")")  |>
        str2lang()  |>
        eval()
    } else {
      values <- NA
    }

    # extract value labels
    value_labels <- .extract_value_labels(
      dic[i, .dic_file$value_labels],
      dic[i, .dic_file$type]
    )

    if (!identical(value_labels, NA)) {
      for (.i in 1:nrow(value_labels)) {
        value <- value_labels[.i, 1]
        names(values)[which(values == value)] <- trimws(value_labels[.i, 2])
      }
    }

    dic_attr(data[[id]], "value_labels") <- value_labels
    dic_attr(data[[id]], "values") <- values

    # extract missing
    dic_attr(data[[id]], .opt$missing) <-
      paste0("c(", as.character(dic[[.dic_file$missing]][i]), ")")  |>
      str2lang() |>
      eval()


    ### assign attributes
    filter <- !(names(.dic_file) %in% c(
      "values", "value_labels", "missing", "variables")
    )
    set <- names(.dic_file)[filter]

    for (j in set) {
      target <- .opt[[j]]
      source <- .dic_file[[j]]
      value_dic <- dic[[source]][i]
      dic_attr(data[[id]], target) <- value_dic
    }

    # set dic attributes for further variables
    new_dic_attributes <- names(dic)[!names(dic) %in% unlist(.dic_file)]

    for(j in new_dic_attributes) {
      dic_attr(data[[id]], j) <- dic[[j]][i]
    }


    ### set factors
    if (factors && dic_attr(data[[id]], "type") == "factor") {
      data[[id]] <- .set_factor(data[[id]])
    }

    class(data[[id]]) <- c("dic", class(data[[id]]))
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
    #msg <- c(msg, "`label` attribute set.")
    data <- dic_haven(data)
  }

  return_messages(msg)

  data
}
