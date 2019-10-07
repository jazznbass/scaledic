#' Apply dictionary
#'
#' @param data Data frame
#' @param dic Dictionary file
#' @param factors If set TRUE, factor variables will be turned into factors.
#' @return A data frame with dictionary information.
#' @examples
#' dat <- apply_dic(ITRF, dic_ITRF)
#' list_scales(dat)
#' @export

apply_dic <- function(data, dic, factors = TRUE) {

  opt.attr <- .dic_file
  names(dic) <- tolower(names(dic))

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
    if (is.na(dic[[.dic_file$item_label_short]][i])) {
      message("Missing label found at variabel no. ", i, " in dic file.")
      # dic[[.dic_file$item_label_short]][i] <- paste0(
      #   dic[[.dic_file$scale]][i], "_",
      #   dic[[.dic_file$subscale]][i], "_",
      #   dic[[.dic_file$index]][i]
      # )
      # message("Replaced missing label to ", dic[[.dic_file$item_label_short]][i])
    }
  }

  #copy label to var when var is missing
  if (!(.dic_file$variable %in% names(dic)))
    dic[[.dic_file$variable]] <- dic[[.dic_file$item_label_short]]

  # checking missing variables in dictionary file
  miss <- unlist(.dic_file)[which(!(unlist(.dic_file) %in% names(dic)))]
  if (length(miss) > 0) {
    miss %>%
      paste(collapse = ", ") %>%
      message("The following variables were missing in the dictionary file: ", .)
    dic[, miss] <- NA
  }
  for (i in 1:nrow(dic)) {
    id <- which(names(data) == dic[[.dic_file$variable]][i])
    if (length(id) == 0) {
      message("Variable ", dic[[.dic_file$variable]][i], " not found in data file.\n")
      next
    }
    names(data)[id] <- dic[[opt.attr$item_label_short]][i]

    ### extract values and value labels
    values <-
      paste0("c(", as.character(dic[i, opt.attr$values]), ")") %>%
      parse(text = .) %>%
      eval()

    value_labels <-
      dic[i, opt.attr$value_labels] %>%
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
      paste0("c(", as.character(dic[[opt.attr$missing]][i]), ")") %>%
      parse(text = .) %>%
      eval()

    ### assign attributes
    filter <- !(names(opt.attr) %in% c("values", "value_labels", "missing", "variables"))
    set <- names(.dic_file)[filter]

    for (j in set) {
      target <- .opt[[j]]
      source <- opt.attr[[j]]
      value_dic <- as.character(dic[[source]][i])
      dic_attr(data[[id]], target) <- value_dic
    }

    ### set factors
    if (factors && dic_attr(data[[id]], .opt$type) == "factor") {
      data[, id] <- factor(
        data[, id],
        levels = dic_attr(data[[id]], .opt$values),
        labels = names(dic_attr(data[[id]], .opt$values))
      )
    }

    dic_attr(data[[id]], .opt$class) <- "item"

  }

  data
}
