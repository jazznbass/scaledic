#' Apply dictionary
#'
#' @param data Data frame
#' @param dic A data frame comprising a dictionary or a character string with a
#' filename (for now an Microsoft Excel file) containg a dictionary.
#' @param factors If set TRUE, factor variables will be turned into factors.
#' @param replace_missing If TRUE, missing values from the dic are replaced with NA
#' @return A data frame with dictionary information.
#' @examples
#' dat <- apply_dic(ITRF, dic_ITRF)
#' list_scales(dat)
#' @export

apply_dic <- function(data, dic, factors = TRUE, set_dic_attr = TRUE, set_label_attr = TRUE, replace_missing = TRUE, score_scales = TRUE, rename_var = NULL) {

  if ("character" %in% class(dic)) dic <- readxl::read_excel(dic)
  if ("character" %in% class(data)) data <- readxl::read_excel(data)

  dic <- dic[apply(dic, 1, function(x) !all(is.na(x))),]

  dic_scores <- data.frame()
  if (.opt$score_filter %in% names(dic)) {
    dic_scores <- dic[!is.na(dic[[.opt$score_filter]]), ]
    dic <- dic[is.na(dic[[.opt$score_filter]]), ]
  }


  #rename dic names
  names(dic) <- tolower(names(dic))
  names(dic)[which(names(dic) %in% c("label", "name"))] <- "item_name"
  names(dic)[which(names(dic) %in% "item")] <- "item_label"
  names(dic)[which(names(dic) %in% "sub_scale_2")] <- "subscale_2"
  names(dic)[which(names(dic) %in% "sub_scale")] <- "subscale"
  names(dic)[which(names(dic) %in% "sub_scale_label")] <- "subscale_label"
  names(dic)[which(names(dic) %in% "sub_scale_2_label")] <- "subscale_2_label"

  #rename data variables
  if (!is.null(rename_var)) {
    to_from <- setNames(dic[[rename_var]], dic[[.dic_file$item_name]])
    for(i in 1:length(to_from))
      names(data)[which(names(data) == to_from[i])] <- names(to_from[i])
  }

  #!old code: copy name to var when var is missing
  #if (is.null(dic[[.dic_file$variable]]))
  #  dic[[.dic_file$variable]] <- dic[[.dic_file$item_name]]

  # check for missing weight variable
  if (is.null(dic[[.dic_file$weight]])) {
    message("'Weight' variable missing in the dictionary file. Variable inserted with default of 1.")
    dic[[.dic_file$weight]] <- 1
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
  if (length(miss) > 0) {
    miss %>%
      paste(collapse = ", ") %>%
      message("The following variables were missing in the dictionary file: ", .)
    dic[, miss] <- NA
  }


  # weight NA to integer
  miss_weight <- which(is.na(dic[[.dic_file$weight]]))
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


  var_not_df <- NULL

  for (i in 1:nrow(dic)) {

    # rename columns in dataframe with label when column name is in dic file var variable
    id <- which(names(data) == dic[[.dic_file$item_name]][i])
    if (length(id) == 0) {
      #message("Variable ", dic[[.dic_file$item_name]][i], " not found in data file.\n")
      var_not_df <- c(var_not_df, dic[[.dic_file$item_name]][i])
      next
    }
    names(data)[id] <- dic[[.dic_file$item_name]][i]

    # extract values and value labels
    values <-
      paste0("c(", as.character(dic[i, .dic_file$values]), ")") %>%
      parse(text = .) %>%
      eval()

    value_labels <-
      dic[i, .dic_file$value_labels] %>%
      as.character() %>%
      strsplit(";") %>%
      unlist() %>%
      strsplit("=")

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

    ### assign attributes
    filter <- !(names(.dic_file) %in% c("values", "value_labels", "missing", "variables"))
    set <- names(.dic_file)[filter]

    for (j in set) {
      target <- .opt[[j]]
      source <- .dic_file[[j]]
      value_dic <- as.character(dic[[source]][i])
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
      temp <- factor(
        data[[id]],
        levels = values,
        labels = names(values)
      )
      attr(temp, .opt$dic) <- attr(data[[id]], .opt$dic)
      data[[id]] <- temp
    }

    dic_attr(data[[id]], .opt$class) <- "item"
    if (set_dic_attr) class(data[[id]]) <- c("dic", class(data[[id]]))
  }

  if (!is.null(var_not_df)) {
    #message("Variables from dic not found in data file: ", paste0(var_not_df, collapse = ", "),"\n")
    message(length(var_not_df), " from ", nrow(dic), " variables from dic not found in data file.\n")
  }


  if (score_scales) data <- score_from_dic(data, dic_scores)


  if (replace_missing) {
    data <- replace_missing(data)
    message("Replaced missing values.")
  }

  if (set_label_attr) {
    message("`label` attribute set.")
    data <- dic_haven(data)
  }
  #class(data) <- c("dic_df", class(data))
  data
}
