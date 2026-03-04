#' Create a dic vector with metadata attributes
#'
#' Factory function to create a `dic` vector with appropriate attributes.
#' This function performs parsing and validation of the provided metadata.
#' It is recommended to use this function to create `dic` objects rather than
#' manually setting attributes.
#'
#' Details:
#' - If `item_name` is missing, it is derived from the variable name in
#'   the calling environment.
#' - If `type` is missing, it is estimated from the class of `x`.
#' - If `values` or `value_labels` are provided, they are parsed
#'   according to the specified `type`.
#' - If `.coerce_class` is TRUE (default), `x` is coerced to match the
#'   specified `type`.
#' - If `type` is "factor", a factor is created from `x` and `values`.
#' - The resulting object is of class `dic` with appropriate attributes set.
#'
#' @param x A vector.
#' @param item_name Character.
#' @param item_label Character.
#' @param values Numeric or character vector with values. The vector can be
#'   named.
#' @param value_labels Character of the form `value = label; value2 = label2`.
#' @param missing Numeric or character vector with values.
#' @param weight numeric.
#' @param type defaults to data type of x.
#' @param recodes Recoding information e.g. `4 = 1, .default = 0`.
#' @param class default is "item".
#' @param ... Additional attributes to be added to the dic attribute list.
#' @param dic_attributes Optional pre-built attribute list (skips parsing if given).
#' @param .coerce_class If TRUE, coerce x to match `type`.
#' @param .format_date Date format for coercion from character -> Date.
#' @export
dic <- function(x,
                item_name = NULL,
                item_label = NULL,
                values = NULL,
                value_labels = NULL,
                missing = NULL,
                weight = 1,
                type = NULL,
                recodes = NULL,
                class = "item",
                ...,
                dic_attributes = NULL,
                .coerce_class = TRUE,
                .format_date = "%Y-%m-%d") {



  # return directly if caller provides attribute list -----
  if (!is.null(dic_attributes)) {
    if (!is.list(dic_attributes)) {
      abort("'dic_attributes' must be a list.")
    }

    # type-based coercion
    if (.coerce_class && has_info(dic_attributes$type)
        && !is.na(dic_attributes$type)) {
      x <- .check_coerce_class(x, dic_attributes, .format_date)
    }
    label  <- dic_attributes$item_label
    labels <- if (!is.null(dic_attributes$values) &&
                  !is.null(names(dic_attributes$values))) {
      dic_attributes$values
    } else {NULL}
    return(
      new_dic(x, dic_attributes = dic_attributes, label = label, labels = labels)
    )
  }

  # derive item_name from call if missing -----
  if (!has_info(item_name)) {
    item_name <- as.character(match.call()[2])
    notify("'item_name' definition is missing and set to '", item_name, "'.")
  }

  # build attributes ------
  attrs <- create_dic_attributes(
    x = x,
    item_name = item_name,
    item_label = item_label,
    values = values,
    value_labels = value_labels,
    missing = missing,
    weight = weight,
    type = type,
    recodes = recodes,
    class = class,
    ...
  )

  # type coercion -----
  if (.coerce_class) {
    x <- .check_coerce_class(x, attrs, .format_date)
  }

  if (attrs$type == "factor") {

    labels <- attrs$values
    if (!has_info(labels)) labels <- sort(unique(x))
    if (is.null(names(labels))) names(labels) <- labels
    nas <- sum(is.na(x))
    prelevels <- unique(x)
    x <- factor(x, levels = labels, labels = names(labels), ordered = FALSE)
    if (sum(is.na(x)) > nas) {
      mislabels <- setdiff(prelevels, labels)
      nas <- sum(is.na(x)) - nas
      notify(
        "Set ", nas, if_one(nas, " NA ", " NAs "),
        "in factor '", attrs$item_name, "' for ",
        if_one(length(mislabels), "value ", "values "),
        paste0(mislabels, collapse = ", ")
      )
    }
  }

  # dic return ----
  label  <- attrs$item_label
  labels <- if (has_info(attrs$values) &&
                !is.null(names(attrs$values))) attrs$values else NULL

  new_dic(x, dic_attributes = attrs, label = label, labels = labels)
}

#' Build and validate dic attribute list
#'
#' @param x Optional. Used for type estimation only if `type` missing.
#' @keywords internal
create_dic_attributes <- function(x = NULL,
                                  item_name = NULL,
                                  item_label = NULL,
                                  values = NULL,
                                  value_labels = NULL,
                                  missing = NULL,
                                  weight = 1,
                                  type = NULL,
                                  recodes = NULL,
                                  class = "item",
                                  ...) {

  dots <- list(...)

  # set defaults for attributes  -----

  prefix <- paste0("'", item_name, "': ")

  if (!has_info(item_label)) {
    notify(prefix, "'item_label' definition is missing and copied from item_name.", detail = 2)
    item_label <- item_name
  }

  if (!has_info(weight)) {
    weight <- 1
    notify(prefix, "'weight' definition is missing and set to 1.", detail = 2)
  }

  if (!has_info(class)) {
    class <- "item"
    notify(prefix, "'class' definition is missing and set to 'item'.")
  }

  # type estimation and check -----
  type <- .check_type(type, x, item_name)

  # parse values/missing/value_labels based on type ------
  if (has_info(type)) {
    values_parsed <- .extract_values(values, type, item = item_name, field = "values")
    missing_parsed <- .extract_values(missing, type, item = item_name, field = "missing")

    value_labels_parsed <- NULL
    if (has_info(value_labels)) {
      value_labels_parsed <- .extract_value_labels(value_labels, type)
    }

    # sync: if labels present but values missing -> take from labels (old behavior)
    if (has_info(value_labels_parsed)) {
      if (!has_info(values_parsed) && has_info(value_labels_parsed[[1]])) {
        values_parsed <- value_labels_parsed[[1]]
        notify(prefix,
          "'values' definition is missing and taken from 'value_labels' definition."
        )
      }

      # apply names(values) from value_labels to values_parsed
      if (has_info(values_parsed) && has_info(value_labels_parsed[[1]])) {
        .id <- sapply(value_labels_parsed[[1]], function(v) {
          id <- which(as.character(v) == as.character(values_parsed))
          if (length(id) == 0) {
            notify(prefix, "Value from 'value_labels' not found in 'values'.")
            return(NA_integer_)
          }
          id
        })
        if (!any(is.na(.id))) {
          names(values_parsed)[.id] <- trimws(value_labels_parsed[[2]])
        }
      } else {
        values_parsed <- values
        missing_parsed <- missing
        value_labels_parsed <- value_labels
      }
    }
  }

  # parse recodes ------
  recodes_parsed <- NULL
  if (has_info(recodes)) recodes_parsed <- .extract_scores(recodes)

  # return ------
  out <- c(
    list(
      item_name = item_name,
      item_label = item_label,
      values = values_parsed,
      value_labels = value_labels_parsed,
      missing = missing_parsed,
      weight = weight,
      type = type,
      class = class
    ),
    dots
  )
  if (has_info(recodes)) out$recodes <- recodes_parsed

  out
}

#' Check and coerce class of x to match type
#' @keywords internal
.check_coerce_class <- function(x, attrs, .format_date) {

  type <- attrs$type

  #if (!has_info(type)) {
  #  notify("Cannot coerce class of x: 'type' definition is missing.")
  #  return(x)
  #}

  pre_NA <- sum(is.na(x))

  prefix <- paste0("dic type defintion of '", attrs$item_name, "' is ")

  if (type == "double" && !is.double(x)) {
    notify(
      prefix, "'double' but variable is of class '",
      paste0(class(x), collapse = ", "), "' --> coerced to 'double'."
    )
    x[] <- suppressWarnings(as.double(x))
  }

  if (type == "integer" && any(x[!is.na(x)] %% 1 != 0)) {
    notify(
      prefix, "'integer' but variable is of class '",
      paste0(class(x), collapse = ", "), "' --> coerced to 'integer'."
    )
    x[] <- suppressWarnings(as.integer(x))
  }

  if (type == "numeric" && !is.numeric(x)) {
    notify(
      prefix, "'numeric' but variable is of class '",
      paste0(class(x), collapse = ", "), "' --> coerced to 'numeric'."
    )
    x[] <- suppressWarnings(as.numeric(x))
  }

  if (type == "character" && !is.character(x)) {
    notify(
      prefix, "'character' but variable is of class '",
      paste0(class(x), collapse = ", "), "' --> coerced to 'character'."
    )
    x[] <- suppressWarnings(as.character(x))
  }

  if (type == "logical" && !is.logical(x)) {
    notify(
      prefix, "'logical' but variable is of class '",
      paste0(class(x), collapse = ", "), "' --> coerced to 'logical'."
    )
    x[] <- suppressWarnings(as.logical(x))
  }

  if (type == "date" && is.character(x)) {
    notify(
      prefix, "'date' but variable is of class 'character' ",
      "--> coerced to 'date' with format ", .format_date
    )
    x <- suppressWarnings(as.Date(x, format = .format_date))
  }

  if (type == "date" &&
      !inherits(x, c("Date", "POSIXct", "POSIXt", "character"))) {
    notify(
      prefix, "'date' but variable is of class '",
      paste0(class(x), collapse = ", "), "' --> coerced to 'character'."
    )
    x[] <- suppressWarnings(as.character(x))
  }

  post_NA <- sum(is.na(x))
  if (post_NA > pre_NA) {
    notify(
      "Coercion to '", type, "' introduced ", post_NA - pre_NA,
      " additional NA values in item '", attrs$item_name, "'."
    )
  }

  x
}

#' Check and estimate type if missing
#' @keywords internal
.check_type <- function(type, x, item_name) {
  if (is.null(x)) {
    if (!has_info(type)) {
      notify(
        "Type of ", item_name,
        " is missing and cannot be estimated (x is NULL) --> set to 'character'."
      )
      type <- "character"
    }
    return(type)
}

  # type estimation --------
  if (!has_info(type)) {
    if (is.numeric(x)) type <- "numeric"
    #if (is.integer(x)) type <- "integer"
    #if (is.double(x)) type <- "numeric"
    if (is.character(x)) type <- "character"
    if (is.factor(x)) type <- "factor"
    if (is.logical(x)) type <- "logical"
    if (inherits(x, c("Date", "POSIXct", "POSIXt"))) type <- "date"
    notify("Type of ", item_name, " is missing and is estimated as '", type, "'.")
  }

  # type check --------

  .types <- c("real", "float", "double")
  if (type %in% .types) {
    type <- "numeric"
    notify("Types ", paste0(.types, collaspe = ", "),
                " are replaced by 'numeric'.", detail = 2)
  }

  type
}


