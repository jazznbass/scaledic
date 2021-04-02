#' Apply dictionary
#'
#' @param data Data frame
#' @param dic A data frame comprising a dictionary or a character string with a
#' filename (for now an Microsoft Excel file) containg a dictionary.
#' @param factors If set TRUE, factor variables will be turned into factors.
#' @param coerce_class If set TRUE mismatches between class and dic type are corrected.
#' @param replace_missing If TRUE, missing values from the dic are replaced with NA
#' @param check_values If TRUE, performs the check_values function on the variables of the data frame included in the dic file.
#' @return A data frame with dictionary information.
#' @examples
#' dat <- apply_dic(ex_itrf, dic_ITRF)
#' descriptives(dat)
#' @export

apply_dic <- function(data, dic, factors = TRUE, set_label_attr = TRUE, coerce_class = TRUE, replace_missing = TRUE, score_scales = TRUE, check_values = FALSE, impute_values = FALSE, rename_var = NULL) {

  if ("character" %in% class(dic)) dic <- readxl::read_excel(dic)
  if ("character" %in% class(data)) data <- readxl::read_excel(data)

  dic <- .clean_dic_file(dic)

# rename variables by rename_var ---------------------------------------------

  if (!is.null(rename_var)) {
    to_from <- setNames(dic[[rename_var]], dic[[.dic_file$item_name]])
    to_from[to_from != ""]
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

  .filter <- dic[[.dic_file$class]] == "scale"
  dic_scores <- dic[.filter, ]
  dic <- dic[!.filter, ]

  var_not_df <- NULL


# apply dic information to variables --------------------------------------

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
    values <- paste0("c(", as.character(dic[i, .dic_file$values]), ")") %>%
      parse(text = .) %>%
      eval()

    # extract value labels
    value_labels <- dic[i, .dic_file$value_labels] %>%
      as.character() %>%
      strsplit(";") %>%
      unlist() %>%
      strsplit("=")

    .n_labels <- length(value_labels)
    .df <- data.frame(
      value = character(.n_labels),
      label = character(.n_labels)
    )
    for(j in 1:.n_labels) {
      .df[j, 1] <- trimws(value_labels[[j]][1])
      .df[j, 2] <- trimws(value_labels[[j]][2])
    }
    if (.dic_file$type %in% c("integer", "numeric", "float", "double"))
      .df[["value"]] <- as.numeric(.df[["value"]])

    dic_attr(data[[id]], .opt$value_labels) <- .df

    for (x in value_labels) {
      value <- as.numeric(x[1])
      names(values)[which(values == value)] <- trimws(x[2])
    }

    dic_attr(data[[id]], .opt$values) <- values

    # extract missing
    dic_attr(data[[id]], .opt$missing) <-
      paste0("c(", as.character(dic[[.dic_file$missing]][i]), ")") %>%
      parse(text = .) %>%
      eval()

    # check variable type (class)
    # numeric:
    if (dic[i, .dic_file$type] %in% c("integer", "numeric", "float", "double")) {
      if (!(class(data[[id]]) %in% c("integer", "numeric", "double"))) {

        if (coerce_class) {
          message(
          paste0("Class should be numeric but is ", class(data[[id]]),
                 ". Coreced to numeric: ", names(data)[id], collapse = ""))
          class(data[[id]]) <- "numeric"
        } else {
          message(
            paste0("Class should be numeric but is ", class(data[[id]]),
                   ": ",names(data)[id], collapse = ""))
        }


      }
    }

    ### assign attributes
    filter <- !(names(.dic_file) %in% c("values", "value_labels", "missing", "variables"))
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
    if (factors && dic_attr(data[[id]], .opt$type) == "factor") {
      values <- dic_attr(data[[id]], .opt$values)
      .factor <- factor(
        data[[id]],
        levels = values,
        labels = names(values)
      )
      .factor <- factor(
        data[[id]],
        levels = dic_attr(data[[id]], .opt$value_labels)$value,
        labels = dic_attr(data[[id]], .opt$value_labels)$label
      )
      attr(.factor, .opt$dic) <- attr(data[[id]], .opt$dic)
      data[[id]] <- .factor
    }

    #dic_attr(data[[id]], .opt$class) <- "item"
    class(data[[id]]) <- c("dic", class(data[[id]]))
  }

  if (!is.null(var_not_df)) {
    message(length(var_not_df), " of ", nrow(dic), " variables from dic not found in data file:")
    message(paste0(var_not_df, collapse = ", "))
  }

  if (replace_missing) {
    data <- replace_missing(data)
    message("Replaced missing values.")
  }

  if (check_values) {
    message("Values checked.")
    vars <- names(data) %in% dic[[.opt$item_name]]
    data[, vars] <- check_values(data[, vars], replace = NA)
  }

  if (score_scales && nrow(dic_scores > 0)) {
    message("Values checked.")
    vars <- names(data) %in% dic[[.opt$item_name]]
    data[, vars] <- check_values(data[, vars], replace = NA)
    if (impute_values) message("Scales imputed.")
    message("Scales scored.")
    data <- score_from_dic(data, dic_scores, impute_values = impute_values)
  }

  if (set_label_attr) {
    message("`label` attribute set.")
    data <- dic_haven(data)
  }
  data
}

.clean_dic_file <- function(dic) {

  # delete empty rows
  dic <- dic[apply(dic, 1, function(x) !all(is.na(x))),]

  # delete rows with comments (first sign of the first column is a #)
  .filter <- which(apply(dic, 1, function(x) substr(x[1], 1, 1) != "#"))
  dic <- dic[.filter, ]

  # filter if variable "active" is available --------------------------------

  if ("active" %in% names(dic)) {
    dic <- dic[which(dic$active == 1), ]
  }


  #rename dic names
  names(dic) <- tolower(names(dic))
  names(dic)[which(names(dic) %in% c("label", "name"))] <- "item_name"
  names(dic)[which(names(dic) %in% "item")] <- "item_label"
  names(dic)[which(names(dic) %in% "sub_scale_2")] <- "subscale_2"
  names(dic)[which(names(dic) %in% "sub_scale")] <- "subscale"
  names(dic)[which(names(dic) %in% "sub_scale_label")] <- "subscale_label"
  names(dic)[which(names(dic) %in% "sub_scale_2_label")] <- "subscale_2_label"

  # check for missing class variable
  if (is.null(dic[[.dic_file$class]])) dic[[.dic_file$class]] <- "item"

  if (.dic_file$score_filter %in% names(dic)) {
    .filter <- which(!is.na(dic[[.dic_file$score_filter]]))
    dic[.filter, .dic_file$class] <- "scale"
  }


  filter_items <- dic[[.dic_file$class]] == "item"

  # check for missing weight variable
  if (is.null(dic[[.dic_file$weight]])) {
    message("'Weight' variable missing in the dictionary file. Variable inserted with default of 1.")
    dic[[.dic_file$weight]] <- NA
    dic[[.dic_file$weight]][filter_items] <- 1
  }

  if(class(dic[[.dic_file$weight]]) != "numeric") {
    message("'weight' variable is not numeric: transformed variable to numeric.")
    dic[[.dic_file$weight]] <- as.numeric(dic[[.dic_file$weight]])
  }

  # check for missing type variable
  if (is.null(dic[[.dic_file$type]])) {
    message("'type' variable missing in the dictionary file. Variable inserted with default of 'integer'.")
    dic[[.dic_file$type]] <- "integer"
  }

  # type NA to integer
  miss_type <- which(is.na(dic[[.dic_file$type]]))
  if (length(miss_type) > 0) {
    message(length(miss_type), " missing types found and replaced with 'integer'.")
    dic[miss_type, .dic_file$type] <- "integer"
  }

  # checking other missing variables in dictionary file
  miss <- unlist(.dic_file)[which(!(unlist(.dic_file) %in% names(dic)))]

  .filter <- !miss %in% c(.dic_file$score_function, .dic_file$score_filter)
  miss <- miss[.filter]
  if (length(miss) > 0) {
    miss %>%
      paste(collapse = ", ") %>%
      message("The following variables were missing in the dictionary file: ", .)
    dic[, miss] <- NA
  }

  # weight NA to integer
  miss_weight <- which(is.na(dic[[.dic_file$weight]]) && filter_items)
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
    message("Missing item_label found at variabel no. ", paste0(missing_label, collapse = ", "), " in dic file.")
  }

  # check for duplicated item_names in dic
  .duplicates <- duplicated(dic[[.dic_file$item_name]])
  if (any(.duplicates)) {
    id <- dic[[.dic_file$item_name]][which(.duplicates)]
    stop(paste0("Item names duplicated in dic-file: ", paste0(id, collapse = ", ")))
  }


  dic
}
