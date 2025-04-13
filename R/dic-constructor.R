#' Dictionary class low level constructor
#'
#' @param x A variable
#' @param item_name Character
#' @param item_label Character
#' @param values Numeric or character vector with values. The vector can be
#'   named
#' @param value_lables Character of the form `value = label; value2 = label2`
#' @param missing Numeric or character vector with values
#' @param weight numeric
#' @param type defaults to "integer"
#' @param class default is "item"
#' @param ... further dic arguments (e.g. `source = "James (1891)"`)
#' @param .list An alternative way to provide class arguments as a list
#'   (overwrites previous arguments)
#' @param .coerce_class Logical. If TRUE, tries to coerce classes of 'x' if
#'   class does not match the `type` argument
#' @param .message_attr Logical. For internal use
#' @return An item of class dic.
#' @export
#'
#' @examples
#' x <- new_dic(1:100, item_label = "The label of this item")
new_dic <- function(x,
                    item_name = NULL,
                    item_label = NULL,
                    values = NULL,
                    value_labels = NULL,
                    missing = NULL,
                    weight = 1,
                    type = NULL,
                    class = "item",
                    ...,
                    .list = NULL,
                    .coerce_class = TRUE,
                    .message_attr = FALSE) {


  msg <- c()

  if (!has_info(item_name)) {
    item_name <- as.character(match.call()[2])
  }

  if (!has_info(item_label)) {
    item_label <- item_name
  }

  if (!has_info(type)) {
    type <- "integer"
    if (is.numeric(x)) type <- "numeric"
    if (is.character(x)) type <- "character"
    if (is.factor(x)) type <- "factor"
  }

  if (has_info(values)) {
    if (length(values) == 1 && inherits(values, "character")) {
      values <- .extract_values(values)
    }
  }

  if (has_info(value_labels)) {
    value_labels <- .extract_value_labels(value_labels, type)
  }

  if (has_info(value_labels)) {
    .id <- sapply(value_labels[[1]], function(x)
      which(as.character(x) == as.character(values)
    ))
    names(values)[.id] <- trimws(value_labels[[2]])
  }


  if (identical(type, "integer") && !is.integer(x) && !is.numeric(x)) {
    if (.coerce_class) {
      msg <- c(msg, paste0(
        "Class should be integer but is ", paste0(class(x), collapse = ", "),
        ". Coerced to integer"
      ))
      x[] <- as.integer(x)
      class(x) <- "integer"
    } else {
      msg <- c(msg, paste0(
        "Class should be integer but is ", paste0(class(x), collapse = ", ")
      ))
    }
  }

  if (type %in% c("float", "double") && !is.double(x) && !is.numeric(x)) {
    if (.coerce_class) {
      msg <- c(msg, paste0(
        "Class should be double but is ", paste0(class(x), collapse = ", "),
        ". Coerced to integer"
      ))
      x[] <- as.double(x)
      class(x) <- "numeric"
    } else {
      msg <- c(msg, paste0(
        "Class should be double but is ", paste0(class(x), collapse = ", ")
      ))
    }
  }

  if (type == "numeric" && !is.numeric(x)) {
      if (.coerce_class) {
        msg <- c(msg, paste0(
          "Class should be numeric but is ", paste0(class(x), collapse = ", "),
          ". Coerced to numeric"
        ))
        x[] <- as.double(x)
        class(x) <- "numeric"
      } else {
        msg <- c(msg, paste0(
          "Class should be numeric but is ", paste0(class(x), collapse = ", ")
        ))
      }
  }

  dic_list <- list(
    item_name = item_name,
    item_label = item_label,
    values = values,
    value_labels = value_labels,
    missing = missing,
    weight = weight,
    type = type,
    class = class,
    ...
  )

  dic_list <- c(.list, dic_list)
  dic_list <- dic_list[unique(names(dic_list))]

  dic_attr(x) <- dic_list
  attr(x, "label") <- dic_attr(x, "item_label")
  attr(x, "labels") <- dic_attr(x, "values")

  if (type == "factor") x <- .set_factor(x)

  class(x) <- c("dic", class(x))

  if (.message_attr) attr(x, "messages") <- msg else return_messages(msg)

  x
}

#' Set dictionary information to variables
#'
#' @param data A data frame or a vector.
#' @param .vars Character vector with variable names. If data is a data frame,
#'   address these variables. If left `NULL` and data is a data frame, all
#'   variables from the data frame are addressed.
#' @param ... dic attributes of the form `attribute = value`.
#' @details Standard attributes are: `"item_name"`, `"item_label"`, `"weight"`,
#'   `"type"`, `"values"`, `"value_labels"`, `"missing"`.
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

  dic <- dic_attr(data)
  dic <- c(parameters, dic)
  dic <- dic[unique(names(dic))]

  if (!"class" %in% names(dic)) dic[["class"]] <- "item"
  if (!"item_name" %in% names(dic)) {
    msg <- c(msg, paste0(
      "Attribute 'item_name' missing and set to '", .item_name, "'."
    ))
    dic[["item_name"]] <- .item_name
    dic[["item_name"]] <- .item_name
  }
  if (!"item_label" %in% names(dic)) {
    msg <- c(msg, paste0(
        "Attribute 'item_label' missing and set to '", dic[["item_name"]], "'."
    ))
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
      msg <- c(msg, paste0(
        "Vector has values not defined as value_labels. ",
        "These are automatically set to NA."
      ))
    }

    data <- factor(
      data,
      levels = dic[["values"]],
      labels = dic[["value_labels"]]$label
    )
  }

  # set dic attributes

  dic_attr(data) <- dic
  attr(data, "label") <- dic_attr(data, "item_label")

  if (!inherits(data, "dic")) class(data) <- c("dic", class(data))

  if (length(msg) > 0) {
    message(paste0(1:length(msg), ": ", msg, "\n"))
  }

  data
}

.extract_value_labels <- function(value_labels, type) {

  if (is.na(value_labels) || value_labels == "") return(NA)
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

.extract_values <- function(x, sep = ";", quotes = FALSE) {
  paste0("c(", x, ")") |> str2lang() |> eval()
  #x <- strsplit(x, sep)[[1]]
  #x <- lapply(x, function(.) trimws(strsplit(. , "=")[[1]]))
  #for(i in seq_along(x)) {
  #  names(x)[i] <- if (quotes) trimws(x[[i]][2], whitespace = "['\"]") else x[[i]][2]
  #  x[[i]] <- x[[i]][1]
  #  suppressWarnings(
  #    if (!is.na(as.double(x[[i]]))) x[[i]] <- as.double(x[[i]])
  #  )
  #}
  #setNames(unlist(x), names(x))
}

.set_factor <- function(x) {
  value_labels <- dic_attr(x, "value_labels")

  # default
  levels <- unique(x)
  labels <- unique(x)

  # sensible values exist
  if (has_info(dic_attr(x, "values"))) {
    levels <- unname(dic_attr(x, "values"))
    labels <- unname(dic_attr(x, "values"))
  }

  # sensible value_labels exist
  #if (!identical(value_labels, NA) && !identical(value_labels, "")) {
  #  levels <- value_labels$value
  #  labels <- value_labels$label
  #}

  out <- factor(
    x,
    levels = levels,
    labels = labels
  )
  dic_attr(out) <- dic_attr(x)
  out
}

has_info <- function(x) {
  if (is.null(x) || isTRUE(is.na(x)) || identical(unname(x), "")) return(FALSE)
  TRUE
}

