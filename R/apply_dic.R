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

  opt.attr <- list(
    "item_label" = "ITEM",
    "item_label_short" = "LABEL",
    "scale" = "SCALE",
    "subscale" = "SUB_SCALE",
    "subscale_2" = "SUB_SCALE_2",
    "scale_label" = "SCALE_LABEL",
    "subscale_label" = "SUB_SCALE_LABEL",
    "subscale_2_label" = "SUB_SCALE_2_LABEL",
    "index" = "INDEX",
    "weight" = "WEIGHT",
    "source" = "SOURCE",
    "note" = "NOTE",
    "type" = "TYPE",
    "values" = "VALUES",
    "value_labels" = "VALUE_LABELS",
    "missing" = "MISSING"
  )

  names(dic) <- toupper(names(dic))

  if (is.null(dic$VAR)) dic$VAR <- dic$LABEL

  miss <- unlist(opt.attr)[which(!(unlist(opt.attr) %in% names(dic)))]
  if (!is.null(miss)) dic[, miss] <- NA

  for (i in 1:nrow(dic)) {
    id <- which(names(data) == dic$VAR[i])

    names(data)[id] <- dic$LABEL[i]

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
    set <- c(
      "item_label", "item_label_short", "scale", "subscale",
      "subscale_2", "scale_label", "subscale_label",
      "subscale_2_label", "index", "weight", "source", "note", "type"
    )

    for (j in set) {
      target <- .opt[[j]]
      source <- opt.attr[[j]]
      dic_attr(data[[id]], target) <- as.character(dic[[source]][i])
    }

    ### set factors
    if (factors && dic_attr(data[[id]], .opt$type) == "factor") {
      data[, id] <- factor(
        data[, id],
        levels = dic_attr(data[[id]], .opt$values),
        labels = names(dic_attr(data[[id]], .opt$values))
      )
    }
  }

  data
}


apply_dic_old <- function(data, dic, factors = TRUE) {
  opt.attr <- list(
    "item_label" = "ITEM",
    "item_label_short" = "LABEL",
    "scale" = "SCALE",
    "subscale" = "SUB_SCALE",
    "subscale_2" = "SUB_SCALE_2",
    "scale_label" = "SCALE_LABEL",
    "subscale_label" = "SUB_SCALE_LABEL",
    "subscale_2_label" = "SUB_SCALE_2_LABEL",
    "index" = "INDEX",
    "weight" = "WEIGHT",
    "source" = "SOURCE",
    "note" = "NOTE",
    "type" = "TYPE",
    "values" = "VALUES",
    "value_labels" = "VALUE_LABELS",
    "missing" = "MISSING"
  )

  names(dic) <- toupper(names(dic))

  if (is.null(dic$VAR)) dic$VAR <- dic$LABEL

  miss <- unlist(opt.attr)[which(!(unlist(opt.attr) %in% names(dic)))]
  if (!is.null(miss)) dic[, miss] <- NA

  for (i in 1:nrow(dic)) {
    id <- which(names(data) == dic$VAR[i])

    names(data)[id] <- dic$LABEL[i]

    ### extract values and value labels
    values <- paste0("c(", as.character(dic[i, opt.attr$values]), ")")
    values <- eval(parse(text = values))
    value_labels <- as.character(dic[i, opt.attr$value_labels])
    value_labels <- unlist(strsplit(value_labels, ";"))
    value_labels <- strsplit(value_labels, "=")
    for (x in value_labels) {
      value <- as.numeric(x[1])
      names(values)[which(values == value)] <- trimws(x[2])
    }

    dic_attr(data[[id]], .opt$values) <- values

    ### extract missing
    missing <- paste0("c(", as.character(dic[[opt.attr$missing]][i]), ")")
    missing <- eval(parse(text = missing))
    dic_attr(data[[id]], .opt$missing) <- missing

    ### assign attributes
    set <- c(
      "item_label", "item_label_short", "scale", "subscale",
      "subscale_2", "scale_label", "subscale_label",
      "subscale_2_label", "index", "weight", "source", "note", "type"
    )

    for (j in set) {
      target <- .opt[[j]]
      source <- opt.attr[[j]]
      dic_attr(data[[id]], target) <- as.character(dic[[source]][i])
    }

    ### set factors
    if (factors && dic_attr(data[[id]], .opt$type) == "factor") {
      data[, id] <- factor(
        data[, id],
        levels = dic_attr(data[[id]], .opt$values),
        labels = names(dic_attr(data[[id]], .opt$values))
      )
    }
  }

  data
}

