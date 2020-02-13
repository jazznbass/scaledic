#' Apply dictionary
#'
#' @param data Data frame
#' @param dic Dictionary file
#' @param factors If set TRUE, factor variables will be turned into factors.
#' @param replace_missing If TRUE, missing values from the dic are replaced with NA
#' @return A data frame with dictionary information.
#' @examples
#' dat <- apply_dic(ITRF, dic_ITRF)
#' list_scales(dat)
#' @export

apply_dic <- function(data, dic, factors = TRUE, set_dic_attr = TRUE, set_label_attr = TRUE, replace_missing = TRUE) {

  #opt.attr <- .dic_file
  names(dic) <- tolower(names(dic))

  #rename dic names
  names(dic)[which(names(dic) %in% "label")] <- "name"

  #copy name to var when var is missing
  if (is.null(dic[[.dic_file$variable]]))
    dic[[.dic_file$variable]] <- dic[[.dic_file$item_name]]

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

  # checking other missing variables in dictionary file
  miss <- unlist(.dic_file)[which(!(unlist(.dic_file) %in% names(dic)))]
  if (length(miss) > 0) {
    miss %>%
      paste(collapse = ", ") %>%
      message("The following variables were missing in the dictionary file: ", .)
    dic[, miss] <- NA
  }

  # type NA to integer
  miss_type <- which(is.na(dic[[.dic_file$type]]))
  if (length(miss_type) > 0) {
    message(length(miss_type), " missing types found and replaced with 'integer'.")
    dic[miss_type, .dic_file$type] <- "integer"
  }

  # weight NA to integer
  miss_weight <- which(is.na(dic[[.dic_file$weight]]))
  if (length(miss_weight) > 0) {
    message(length(miss_weight), " missing weights found and replaced with 1.")
    dic[miss_weight, .dic_file$weight] <- 1
  }

  # set default name when label is missing
  for (i in 1:nrow(dic)){
    if (is.na(dic[[.dic_file$item_label]][i])) {
      message("Missing label found at variabel no. ", i, " in dic file.")
    }
  }

  for (i in 1:nrow(dic)) {
    id <- which(names(data) == dic[[.dic_file$variable]][i])
    if (length(id) == 0) {
      message("Variable ", dic[[.dic_file$variable]][i], " not found in data file.\n")
      next
    }
    names(data)[id] <- dic[[.dic_file$item_name]][i]

    ### extract values and value labels
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

    ### extract missing
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
