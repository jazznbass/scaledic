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

  if (!(opt.attr$variable %in% names(dic)))
    dic[[opt.attr$variable]] <- dic[[opt.attr$item_label_short]]

  # checking missing variables in dictionary file
  miss <- unlist(opt.attr)[which(!(unlist(opt.attr) %in% names(dic)))]
  if (length(miss) > 0) {
    miss %>%
      paste(collapse = ", ") %>%
      message("The following variables were missing in the dictionary file: ", ., "\n")
    dic[, miss] <- NA
  }
  for (i in 1:nrow(dic)) {
    id <- which(names(data) == dic[[opt.attr$variable]][i])

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
