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

  if (inherits(dic, "character")) dic <- .read_by_suffix(dic)
  if (inherits(data, "character")) data <- .read_by_suffix(data)

  dic <- .clean_dic_file(dic)

# rename variables by rename_var ---------------------------------------------

  if (!is.null(rename_var)) {
    to_from <- setNames(dic[[rename_var]], dic[[.dic_file$item_name]])
    to_from <- to_from[!is.na(to_from)]
    .duplicates <- names(to_from) %in% names(data)
    if(any(.duplicates)){
      message("Skipped renaming column to an already existing name: ")
      message(paste0(names(to_from)[.duplicates], collapse = ", "))
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
    ord <- c("item_name", "scale", "subscale", "scale_label", "subscale_label",
      "item_label", "type", "score_filter", "score_function", "class")
    filter <- unlist(lapply(dic_scores, function(x) !all(is.na(x))))
    filter <- sort(names(dic_scores)[filter])
    dic_scores <- dic_scores[, filter]
    attr(data, opt("dic")) <- list(scales = dic_scores)
    #for (i in 1:nrow(dic_scores)) {
    #  new_name <- dic_scores[[opt("item_name")]][i]
    #  data[[new_name]] <- NA
    #  dic_attributes <- as.list(dic_scores[i, ])
    #  dic_attributes[[.dic_file$class]]<- "score"
    #  attr(data[[new_name]], "dic") <- dic_attributes
    #  class(data[[new_name]]) <- c(opt("dic"), "numeric")
    #}
  }

# apply dic information to variables --------------------------------------

  var_not_df <- NULL

  for (i in 1:nrow(dic)) {
    # look up column in data frame corresponding to item_name
    id <- which(names(data) == dic[[.dic_file$item_name]][i])
    if (length(id) > 1) {
      message("Multiple ids found")
      message(paste0(names(data)[id], collapse = ", "))
    }

    if (length(id) == 0) {
      var_not_df <- c(var_not_df, dic[[.dic_file$item_name]][i])
      next
    }

    # extract values
    values <- paste0("c(", as.character(dic[i, .dic_file$values]), ")")  |>
      str2lang()  |>
      eval()

    # extract value labels
    value_labels <- .extract_value_labels(
      dic[i, .dic_file$value_labels],
      dic[i, .dic_file$type]
    )

    for (.i in 1:nrow(value_labels)) {
      value <- value_labels[.i, 1]
      names(values)[which(values == value)] <- trimws(value_labels[.i, 2])
    }

    dic_attr(data[[id]], "value_labels") <- value_labels
    dic_attr(data[[id]], "values") <- values

    # extract missing
    dic_attr(data[[id]], .opt$missing) <-
      paste0("c(", as.character(dic[[.dic_file$missing]][i]), ")")  |>
      str2lang() |>
      eval()

    # check variable type (class)
    # numeric:
    if (dic[i, .dic_file$type] %in% c("integer","numeric","float","double")) {
      if (!inherits(data[[id]], c("integer","numeric","double"))) {

        if (coerce_class) {
          message(
            "Class should be numeric but is ", class(data[[id]]),
            ". Corrected to numeric: ", names(data)[id]
          )
          class(data[[id]]) <- "numeric"
        } else {
          message(
           "Class should be numeric but is ", class(data[[id]]),
           ": ",names(data)[id]
          )
        }
      }
    }

    ### assign attributes
    filter <- !(names(.dic_file) %in% c(
      "values", "value_labels", "missing", "variables")
    )
    set <- names(.dic_file)[filter]

    for (j in set) {
      target <- .opt[[j]]
      source <- .dic_file[[j]]
      #value_dic <- as.character(dic[[source]][i])
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
      #values <- dic_attr(data[[id]], .opt$values)
      #.factor <- factor(
      #  data[[id]],
      #  levels = values,
      #  labels = names(values)
      #)

      value_labels <- dic_attr(data[[id]], "value_labels")

      if (!all(unique(data[[id]]) %in% value_labels$value)) {
        message(names(data)[id], " has values not defined as value_labels. ",
        "These are automatically set to NA.")
      }

      .factor <- factor(
        data[[id]],
        levels = value_labels$value,
        labels = value_labels$label
      )
      attr(.factor, .opt$dic) <- attr(data[[id]], opt("dic"))
      data[[id]] <- .factor
    }

    #dic_attr(data[[id]], .opt$class) <- "item"
    class(data[[id]]) <- c("dic", class(data[[id]]))
  }

  # var not found messages ------

  if (!is.null(var_not_df)) {
    message(
      length(var_not_df), " of ", nrow(dic),
      " variables from dic not found in data file:"
    )
    message(paste0(var_not_df, collapse = ", "))
  }

  # replace missing ----
  if (replace_missing) {
    data <- replace_missing(data)
    message("Replaced missing values.")
  }

  # check values ----
  if (check_values) {
    message("Values checked.")
    vars <- names(data) %in% dic[[.opt$item_name]]
    data[, vars] <- check_values(data[, vars], replace = NA)
  }

  # score scales ----
  if (score_scales && nrow(dic_scores > 0)) {
    message("Values checked.")
    vars <- names(data) %in% dic[[.opt$item_name]]
    data[, vars] <- check_values(data[, vars], replace = NA)
    if (impute_values) message("Scales imputed.")
    message("Scales scored.")
    data <- score_from_dic(data, impute_values = impute_values)
  }

  # return ----

  if (set_label_attr) {
    message("`label` attribute set.")
    data <- dic_haven(data)
  }
  data
}

.clean_dic_file <- function(dic) {

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
  dic[[.column]] <- trimws(dic[[.column]])
  dic[[.column]][which(dic[[.column]] == "")] <- NA

  dic[[.dic_file$item_name]] <- trimws(dic[[.dic_file$item_name]])

  # check for missing class variable
  if (is.null(dic[[.dic_file$class]])) dic[[.dic_file$class]] <- "item"

  if (.dic_file$score_filter %in% names(dic)) {
    .filter <- which(!is.na(dic[[.dic_file$score_filter]]))
    dic[.filter, .dic_file$class] <- "scale"
  }

  filter_items <- dic[[.dic_file$class]] == "item"

  # check for missing weight variable
  if (is.null(dic[[.dic_file$weight]])) {
    message("'Weight' variable missing in the dictionary file. ",
            "Variable inserted with default of 1.")
    dic[[.dic_file$weight]] <- NA
    dic[[.dic_file$weight]][filter_items] <- 1
  }

  if(!inherits(dic[[.dic_file$weight]], "numeric")) {
    message("'weight' variable is not numeric: transformed variable to numeric.")
    dic[[.dic_file$weight]] <- as.numeric(dic[[.dic_file$weight]])
  }

  # check for missing type variable
  if (is.null(dic[[.dic_file$type]])) {
    message("'type' variable missing in the dictionary file. ",
            "Variable inserted with default of 'integer'.")
    dic[[.dic_file$type]] <- "integer"
  }

  # type NA to integer
  miss_type <- which(is.na(dic[[.dic_file$type]]))
  if (length(miss_type) > 0) {
    message(
      length(miss_type), " missing types found and replaced with 'integer'."
    )
    dic[miss_type, .dic_file$type] <- "integer"
  }

  # checking other missing variables in dictionary file
  miss <- unlist(.dic_file)[which(!(unlist(.dic_file) %in% names(dic)))]

  .filter <- !miss %in% c(.dic_file$score_function, .dic_file$score_filter)
  miss <- miss[.filter]
  if (length(miss) > 0) {
    miss %>%
      paste(collapse = ", ")  %>%
      message(
        "The following variables were missing in the dictionary file: ", .
      )
    dic[, miss] <- NA
  }

  # weight NA to integer
  miss_weight <- which(is.na(dic[[.dic_file$weight]]) & filter_items)
  if (length(miss_weight) > 0) {
    message(length(miss_weight), " missing weights found and replaced with 1.")
    dic[miss_weight, .dic_file$weight] <- 1
  }

  # set report missing label
  missing_label <- NULL
  for (i in 1:nrow(dic)){
    if (is.na(dic[[.dic_file$item_label]][i])) missing_label <- c(missing_label, i)
  }
  if (!is.null(missing_label)) {
    message(
      "Missing item_label found at variabel no. ",
      paste0(missing_label, collapse = ", "), " in dic file."
    )
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
  if (any(grepl("\r\n", "; ", x = dic[[.dic_file$value_labels]]))) {
    message("Found linebreaks in value_labels and replaced them with ';'")
    dic[[.dic_file$value_labels]] <- gsub("\r\n", "; ", x =  dic[[.dic_file$value_labels]])
  }


  dic
}
