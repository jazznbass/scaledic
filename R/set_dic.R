#' Set dictionary information to variables
#'
#' @param data A data frame or a vector.
#' @param .vars Character vector with variable names. If data is a data frame,
#'   address these variables. If left `NULL` and data is a data frame, all
#'   variables from the data frame are addressed.
#' @param ... dic attributes of the form `attribute = value`.
#' @details Standard attributes are: `"item_name"`, `"item_label"`, `"scale"`,
#'   `"subscale"`, `"subscale_2"`, `"scale_label"`, `"subscale_label"`,
#'   `"subscale_2_label"`, `"weight"`, `"source"`, `"type"`, `"values"`,
#'   `"value_labels"`, `"missing"`.
#' @return A data frame or a vector with added dic attributes.
#' @examples
#' hap_1 <- sample(1:5, 30, replace = TRUE)
#' set_dic(hap_1,
#'    item_label = "How do you feel today?",
#'    scale = "hap",
#'    scale_label = "Happiness",
#'    values = 1:3,
#'    value_labels = "1 = not happy; 2 = in between; 3 = happy"
#' )
#' @export
set_dic <- function(data, .vars = NULL, ...) {

  parameters <- list(...)

  if (inherits(data, "data.frame")) {
    if (is.null(.vars)) .vars <- names(data)
    for(i in seq_along(.vars)) {
      data[[.vars[i]]] <- .set_dic(data[[.vars[i]]], parameters, .vars[i])
    }
    return(data)
  }

  .set_dic(data, parameters, .item_name = as.character(match.call()[2]))
}

.set_dic <- function(data, parameters, .item_name = "") {

  msg <- c()

  dic <- attr(data, opt("dic"))
  dic <- c(parameters, dic)
  dic <- dic[unique(names(dic))]

  if (!"class" %in% names(dic)) dic[["class"]] <- "item"
  if (!"item_name" %in% names(dic)) {
    msg <- c(
      msg, paste0("Attribute 'item_name' missing and set to '", .item_name, "'.")
    )
    dic[["item_name"]] <- .item_name
  }
  if (!"item_label" %in% names(dic)) {
    msg <- c(
      msg,
      paste0(
        "Attribute 'item_label' missing and set to '", dic[["item_name"]], "'."
      )
    )
    dic[["item_label"]] <- dic[["item_name"]]
  }
  if (!"type" %in% names(dic)) {
    msg <- c(msg, "Attribute 'type' missing and set to 'integer'.")
    dic[["type"]] <- "integer"#class(data)
  }
  if (!"weight" %in% names(dic)) {
    msg <- c(msg, "Attribute 'weight' missing and set to 1.")
    dic[["weight"]] <- 1
  }

  if ("value_labels" %in% names(dic)) {
    value_labels <- .extract_value_labels(dic[["value_labels"]], dic[["type"]])
    for (i in 1:nrow(value_labels)) {
      value <- as.numeric(value_labels[i, 1])
      .id <- which(dic[["values"]] == value)
      names(dic[["values"]])[.id] <- trimws(value_labels[i, 2])
    }
    dic[["value_labels"]] <- value_labels
  }

  # define factors

  if (dic[["type"]] == "factor") {
    if (!all(unique(data) %in% dic[["value_labels"]]$value)) {
      msg <- c(msg, "Vector has values not defined as value_labels. These are automatically set to NA.")
    }

    data <- factor(
      data,
      levels = dic[["values"]],
      labels = dic[["value_labels"]]$label
    )

    #data <- factor(
    #  data,
    #  levels = dic[["value_labels"]]$value,
    #  labels = dic[["value_labels"]]$label
    #)
  }

  # set dic attributes

  attr(data, opt("dic")) <- dic
  attr(data, "label") <- dic_attr(data, "item_label")

  if (!"dic" %in% class(data)) class(data) <- c("dic", class(data))

  if (length(msg) > 0) {
    message(paste0(1:length(msg), ": ", msg, "\n"))
  }

  data
}

.extract_value_labels <- function(value_labels, type) {

  value_labels <- value_labels %>%
    as.character() %>%
    strsplit(";") %>%
    unlist() %>%
    strsplit("=")

  .n_labels <- length(value_labels)
  out <- data.frame(
    value = character(.n_labels),
    label = character(.n_labels)
  )
  for(j in 1:.n_labels) {
    out[j, 1] <- trimws(value_labels[[j]][1])
    out[j, 2] <- trimws(value_labels[[j]][2])
  }
  if (type %in% c("integer", "numeric", "float", "double"))
    out[["value"]] <- as.numeric(out[["value"]])

  out

}

.set_factor <- function(data) {
  if (dic_attr(data, "type") == "factor") {
    values <- dic_attr(data, "values")
    .factor <- factor(
      data,
      levels = values,
      labels = names(values)
    )
    out <- factor(
      data,
      levels = dic_attr(data, "value_labels")$value,
      labels = dic_attr(data, "value_labels")$label
    )
    attr(out, opt("dic")) <- attr(data, opt("dic"))
    out
  }
}
