#' Dictionary class low level constructor
#'
#' This is a low-level constructor for dic objects.
#'
#' Standard attributes are: `"item_name"`, `"item_label"`, `"weight"`, `"type"`,
#' `"values"`, `"value_labels"`, `"missing"`, `"recodes"`.
#'
#' @seealso `set_dic()`
#' @param x A variable
#' @param item_name Character
#' @param item_label Character
#' @param values Numeric or character vector with values. The vector can be
#'   named
#' @param value_labels Character of the form `value = label; value2 = label2`
#' @param missing Numeric or character vector with values
#' @param weight numeric
#' @param type defaults to data type of x
#' @param recodes Recoding information e.g. `4 = 1, .default = 0`
#' @param class default is "item"
#' @param ... further dic arguments (e.g. `source = "James (1891)"`)
#' @param .coerce_class Logical. If TRUE, tries to coerce classes of 'x' if
#'   class does not match the `type` argument
#' @param .format_date Optional string that is applied when character variable is
#'   coerced to a 'Date' class.
#' @return An item of class dic.
#' @export
#' @keywords internal
#' @examples
#' x <- new_dic(
#'   sample(c(1:5, -99), 20, replace = TRUE),
#'   item_name = "My item",
#'   item_label = "The label of this item",
#'   values = "1:5",
#'   value_labels = "1 = no; 2 = mhh; 3 = Okish; 4 = good; 5 = mighty",
#'   missing = "-99"
#' )
#' x
new_dic <- function(x,
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
                    .coerce_class = TRUE,
                    .format_date = "%Y-%m-%d") {


  init_messages(); on.exit(print_messages())

  # validate/ normalize missing attributes -----

  if (!has_info(item_name)) {
    item_name <- as.character(match.call()[2])
    add_message(
      "'item_name' definition is missing and set to '", item_name, "'."
    )
  }
  if (!has_info(item_label)) {
    add_message(
      "'item_label' definition is missing and copied from item_name."
    )
    item_label <- item_name
  }
  if (!has_info(weight)) {
    weight <- 1
    add_message("'weight' definition is missing and set to 1.")
  }
  if (!has_info(class)) {
    class <- "item"
    add_message("'class' definition is missing and set to 'item'.")
  }

  # check and estimate type ----
  type <- .check_type(type, x)

  # check and coerce class of x -----
  if (.coerce_class) {
    x <- .check_coerce_class(x, type, .format_date, item_name)
  }
  class(x) <- unique(c("dic", class(x)))

  # extract values, value_labels, missing  --------

  values <- .extract_values(values, type, item_name, field = "values")
  missing_values <- .extract_values(missing, type, item_name, field = "missing")

  if (has_info(value_labels)) {
    value_labels <- .extract_value_labels(value_labels, type)
  }

  if (has_info(value_labels)) {

    if (!has_info(values) && has_info(value_labels$value)) {
      values <- value_labels$value
      add_message("'values' defintion for item '", item_name,
                  "' is missing and taken from 'value_labels' definition.")
    }
    .id <- sapply(value_labels$value, function(x) {
      id <- which(as.character(x) == as.character(values))
      if (length(id) == 0) {
        add_message(
          "Value from 'value_labels' definition not found in 'values' ",
          "definition for item '", item_name, "'.", frame = -4
        )
        return(NA)
      }
      id
    })
    if (!any(is.na(.id))) names(values)[.id] <- trimws(value_labels$label)
  }

  # when type is factor, create factor -----
  if (type == "factor") x <- .set_factor(x)

  # extract recodes --------
  if (has_info(recodes)) {
    recodes <- .extract_scores(recodes)
  }

  # set list of dic-attributes ----
  dic_list <- list(
    item_name = item_name,
    item_label = item_label,
    values = values,
    value_labels = value_labels,
    missing = missing_values,
    weight = weight,
    type = type,
    class = class,
    ...
  )
  if (has_info(recodes)) dic_list$recodes <- recodes

  dic_attr(x) <- dic_list

  # add haven labels ----
  attr(x, "label") <- dic_attr(x, "item_label")
  if (has_info(values) && !is.null(names(values)))
    attr(x, "labels") <- dic_attr(x, "values")

  x
}

.extract_value_labels <- function(value_labels, type) {

  char_split <- getOption("scaledic.string.split")

  if (grepl("\r\n", x = value_labels)) {
    add_message(
      "Found linebreaks in 'value_labels' definition and replaced them with '",
      char_split, "'", frame = -2
    )
    value_labels <- gsub("\r\n", char_split, x = value_labels)
  }

  if (is.na(value_labels) || value_labels == "") return(NA)

  if (count_chars("=", value_labels) == 0) {
    add_message("No equal sign found in value_labels. Entry is misspecified.",
                frame = -2)
    return(NA)
  }

  if (count_chars("=", value_labels) - count_chars(char_split, value_labels) != 1) {
    add_message("value_labels entry is misspecified ",
                "(you must use ", char_split, " as a separator for value labels).",
                frame = -2)
    return(NA)
  }

  value_labels <- value_labels  |>
    as.character() |>
    strsplit(char_split) |>
    unlist() |>
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

  if (any(is.na(out))) {
    add_message("value_labels entry is misspecified ",
                "(Did you forget to add a label after an = sign?).")
    return(NA)
  }

  if (type %in% c("integer", "numeric", "float", "double")) {
    id_na <- which(
      suppressWarnings(sapply(out[["value"]], function(y) is.na(as.numeric(y))))
    )
    if (length(id_na) > 0) {
      add_message(
        if_one(id_na, "Name", "Names"),
        " from value_label ", if_one(id_na, "is", "are"), " not numeric: ",
        paste0(out[["value"]][id_na], collapse = ", "), " ",
        if_one(id_na, "is", "are"), " not applied."
      )
    }
    out[["value"]] <- suppressWarnings(as.numeric(out[["value"]]))
    if (length(id_na) > 0) out <- out[-id_na,]
  }

  out

}


.extract_scores <- function(recodes) {

  char_split <- getOption("scaledic.string.split")

  if (!has_info(recodes)) return(NULL)
  recodes <- recodes |>
    as.character() |>
    strsplit(char_split) |>
    unlist() |>
    strsplit("=")

  .n_labels <- length(recodes)
  out <- data.frame(
    value = character(.n_labels),
    recode = character(.n_labels)
  )
  for(j in 1:.n_labels) {
    out[j, 1] <- trimws(recodes[[j]][1])
    out[j, 2] <- trimws(recodes[[j]][2])
  }

  out

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

  out <- factor(
    x,
    levels = levels,
    labels = labels
  )
  dic_attr(out) <- dic_attr(x)
  class(out) <- c("dic", "factor")
  out
}

#' Parse a `values` specification from a dic file
#'
#' Supported syntax:
#' - Integers: "1, 2, 3"
#' - Integer ranges: "5:11" (inclusive)
#' - Mixed: "1, 3:5, 9"
#' - Float: "min, max" (two numeric values)
#' - Character/factor: "'m', 'f', 'd'" or "\"m\", \"f\""
#'
#' No code evaluation is performed.
#'
#' @keywords internal
.extract_values <- function(values,
                            type,
                            item,
                            field) {

  if (!has_info(values)) return(NULL)
  values <- trimws(as.character(values))
  if (!nzchar(values)) return(NULL)

  msg_prefix <- paste0(" in item '", item, "' field '", field, "'")

  values <- trimws(unlist(strsplit(values, ",")))
  values <- values[nzchar(values)]

  if (!length(values)) {
    add_message("no entries found for values", msg_prefix, frame = -2)
    return(NULL)
  }

  if (type %in% opt("numerics")) {
    values <- .expand_colon(values, msg_prefix, type = type)
    if (!is.null(values)) values <- suppressWarnings(as.numeric(values))
  }

  if (type == "factor") {
    values <- .expand_colon(values, msg_prefix, type = type)
  }

  if (type %in% c("character", "factor")) {

    if (is.numeric(values)) {
      add_message(type, " 'values' defintions need quotes -> added quotes",
                  frame = -2
      )
      values <- sapply(values, function(x) paste0("'", x, "'"))
    }
    # Must be fully quoted, no embedded quotes of same type
    unquote_one <- function(p) {

      if (grepl("^'.*'$", p)) {
        inner <- substr(p, 2, nchar(p) - 1)
        if (grepl("'", inner, fixed = TRUE)) {
          add_message("single-quoted values must not contain unescaped single ",
                      "quotes", msg_prefix, frame = -3)
          return(NA_character_)
        }
        return(inner)
      }
      if (grepl('^".*"$', p)) {
        inner <- substr(p, 2, nchar(p) - 1)
        if (grepl('"', inner, fixed = TRUE)) {
          add_message("double-quoted values must not contain unescaped double ",
                      "quotes", msg_prefix, frame = -3)
          return(NA_character_)
        }
        return(inner)
      }
      add_message(
        "Character values should be quoted with single or double quotes ",
        "-> added quotes", msg_prefix, frame = -3
      )
      return(paste0("'", p, "'"))
    }
    values <- vapply(values, unquote_one, character(1))
  }

  # ---------- numeric / integer / double / float ----------

  if (type %in% opt("numerics")) {

    # float: exactly two numeric values (min, max)
    if (field == "values" && type == "double") {
      if (length(values) != 2) {
        add_message("Type 'double' requires exactly two numbers for min and max",
                    msg_prefix, frame = -2)
        return(NULL)
      }
      values <- suppressWarnings(as.numeric(values))

      if (values[1] > values[2]) {
        add_message("min must be <= max", msg_prefix, frame = -2)
        return(NULL)
      }
    }

  }

  if (any(is.na(values))) {
    add_message("could not parse some values", msg_prefix, frame = -3)
    return(NULL)
  }

  # integer: all values must be integers
  if (type == "integer") {
    if (any(values %% 1 != 0)) {
      add_message("type 'integer' cannot contain non-integer values", msg_prefix,
                  frame = -2)
    }
  }

  values <- unique(values)
  values
}


.expand_colon <- function(values, msg_prefix, type) {
  parse_number <- function(s) {
    s <- trimws(s)
    if (is.na(suppressWarnings(as.numeric(s)))) {
      if (type %in% opt("numerics")) {
        add_message("could not parse numeric value: ", s, msg_prefix, frame = -4)
        return(NA_real_)
      }
      return(s)
    }
    suppressWarnings(as.numeric(s))
  }

  out <- c()
  for (t in values) {
    t <- trimws(t)

    # range?
    if (grepl(":", t, fixed = TRUE)) {
      # strict: single colon, integer endpoints
      if (length(gregexpr(":", t, fixed = TRUE)[[1]]) != 1) {
        add_message("invalid range token ", t, msg_prefix, frame = -3)
        return(NULL)
      }
      ab <- trimws(unlist(strsplit(t, ":", fixed = TRUE)))
      if (length(ab) != 2) {
        add_message("invalid range token ", t, msg_prefix, frame = -3)
        return(NULL)
      }

      a <- parse_number(ab[1]); b <- parse_number(ab[2])
      if (any(is.na(c(a, b)))) {
        add_message("range endpoints must be numeric: ", t, msg_prefix, frame = -3)
        return(NULL)
      }

      if (a %% 1 != 0 || b %% 1 != 0) {
        add_message("range endpoints must be integers: ", t, msg_prefix, frame = -3)
        return(NULL)
      }

      a <- as.integer(a); b <- as.integer(b)
      if (a <= b) out <- c(out, seq.int(a, b)) else out <- c(out, seq.int(a, b, by = -1L))
      next
    }

    # single number
    n <- parse_number(t)
    if (is.na(n)) {
      add_message("invalid numeric token: ", t, msg_prefix, frame = -3)
      return(NULL)
    }
    out <- c(out, n)
  }
  out
}


.check_coerce_class <- function(x, type, .format_date, item_name) {

  pre_NA <- sum(is.na(x))

  if (type == "double" && !is.double(x)) {
    add_message(
      "'type' defintion is 'double' but variable is of class '",
      paste0(class(x), collapse = ", "), "' --> coerced to 'double'."
    )
    x[] <- suppressWarnings(as.double(x))
  }

  if (type == "integer" && any(x[!is.na(x)] %% 1 != 0)) {
    add_message(
      "'type' defintion is 'integer' but variable is of class '",
      paste0(class(x), collapse = ", "), "' --> coerced to 'integer'."
    )
    x[] <- suppressWarnings(as.integer(x))
  }

  if (type == "numeric" && !is.numeric(x)) {
    add_message(
      "'type' defintion is 'numeric' but variable is of class '",
      paste0(class(x), collapse = ", "), "' --> coerced to 'numeric'."
    )
    x[] <- suppressWarnings(as.numeric(x))
  }

  if (type == "character" && !is.character(x)) {
    add_message(
      "'type' defintion is 'character' but variable is of class '",
      paste0(class(x), collapse = ", "), "' --> coerced to 'character'."
    )
    x[] <- suppressWarnings(as.character(x))
  }

  if (type == "logical" && !is.logical(x)) {
    add_message(
      "'type' defintion is 'logical' but variable is of class '",
      paste0(class(x), collapse = ", "), "' --> coerced to 'logical'."
    )
    x[] <- suppressWarnings(as.logical(x))
  }

  if (type == "date" && is.character(x)) {
    add_message(
      "'type' defintion is 'date' but variable is of class 'character' ",
      "--> coerced to 'date' with format ", .format_date
    )
    x <- suppressWarnings(as.Date(x, format = .format_date))
  }

  if (type == "date" &&
      !inherits(x, c("Date", "POSIXct", "POSIXt", "character"))) {
    add_message(
      "'type' defintion is 'date' but variable is of class '",
      paste0(class(x), collapse = ", "), "' --> coerced to 'character'."
    )
    x[] <- suppressWarnings(as.character(x))
  }
  post_NA <- sum(is.na(x))
  if (post_NA > pre_NA) {
    add_message(
      "Coercion to '", type, "' introduced ", post_NA - pre_NA,
      " additional NA values in item '", item_name, "'."
    )
  }

  x
}

.check_type <- function(type, x) {
  # type estimation --------
  if (!has_info(type)) {
    type <- "numeric"
    if (is.numeric(x)) type <- "numeric"
    if (is.integer(x)) type <- "integer"
    if (is.double(x)) type <- "double"
    if (is.character(x)) type <- "character"
    if (is.factor(x)) type <- "factor"
    if (is.logical(x)) type <- "logical"
    if (inherits(x, c("Date", "POSIXct", "POSIXt"))) type <- "date"
    add_message("Type is missing and is estimated as ", "'", type, "'.", frame = -2)
  }

  # type check --------

  if (type %in% c("real", "float", "numeric")) {
    type <- "double"
    add_message("Type 'real' or 'float' is replaced by 'double'.")
  }

  type
}

