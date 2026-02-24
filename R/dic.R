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

  init_messages(); on.exit(print_messages())

  # allow fast path: caller provides fully prepared attributes
  if (!is.null(dic_attributes)) {
    stopifnot(is.list(dic_attributes))
    # (optional) type-based coercion still possible
    if (.coerce_class && has_info(dic_attributes$type)
        && !is.na(dic_attributes$type)) {
      x <- .check_coerce_class(
        x, dic_attributes$type, .format_date, dic_attributes$item_name
      )
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

  # derive item_name from call if missing
  if (!has_info(item_name)) {
    item_name <- as.character(match.call()[2])
    add_message("'item_name' definition is missing and set to '", item_name, "'.")
  }

  # build attributes (parsing/validation)

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
    ...,
    validate = TRUE
  )

  # optional coercion based on attrs$type
  if (.coerce_class && has_info(attrs$type) && !is.na(attrs$type)) {
    x <- .check_coerce_class(x, attrs$type, .format_date, attrs$item_name)
  }

  # factor handling (previously: if (type=="factor") x <- .set_factor(x))
  # Here: build factor from x + attrs$values if needed.
  if (identical(attrs$type, "factor") && !is.factor(x)) {
    lev <- unique(x)
    if (has_info(attrs$values)) lev <- unname(attrs$values)
    x <- factor(x, levels = lev, labels = lev)
  }

  # haven label/labels
  label  <- attrs$item_label
  labels <- if (has_info(attrs$values) &&
                !is.null(names(attrs$values))) attrs$values else NULL

  new_dic(x, dic_attributes = attrs, label = label, labels = labels)
}

#' Parse a `value_labels` specification from a dic file
#' Supported syntax
#' - "1 = 'Yes'; 2 = 'No'; 3 = 'Maybe'"
#' - "1 = Yes; 2 = No; 3 = Maybe"
#' - "
#' 1 = 'Yes'
#' 2 = 'No'
#' 3 = 'Maybe'
#' "
#' No code evaluation is performed.
#' @keywords internal
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

#' Parse a `recodes` specification from a dic file
#' Supported syntax:
#' - "1 = 5, 2 = 4, 3 = 3, 4 = 2, 5 = 1"
#' - "1 = 0, 2 = 0, 3 = 1, 4 = 1, .default = 9"
#' No code evaluation is performed.
#' @keywords internal
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
                      "quotes", msg_prefix, frame = -4)
          return(NA_character_)
        }
        return(inner)
      }
      if (grepl('^".*"$', p)) {
        inner <- substr(p, 2, nchar(p) - 1)
        if (grepl('"', inner, fixed = TRUE)) {
          add_message("double-quoted values must not contain unescaped double ",
                      "quotes", msg_prefix, frame = -4)
          return(NA_character_)
        }
        return(inner)
      }
      add_message(
        "Character values should be quoted with single or double quotes ",
        "-> added quotes", msg_prefix, frame = -4
      )
      return(paste0("'", p, "'"))
    }
    values <- vapply(values, unquote_one, character(1))
  }

  # ---------- numeric / integer / double / float ----------

  if (type %in% opt("numerics")) {

    # float: exactly two numeric values (min, max)
    #if (field == "values" && type == "double") {
    #  if (length(values) != 2) {
    #    add_message("Type 'double' expects exactly two numbers for min and max",
    #                msg_prefix, frame = -5)
    #    return(NULL)
    #  }
    #  values <- suppressWarnings(as.numeric(values))
    #
    #  if (values[1] > values[2]) {
    #    add_message("min must be <= max", msg_prefix, frame = -4)
    #    return(NULL)
    #  }
    #}

  }

  if (any(is.na(values))) {
    add_message("could not parse some values", msg_prefix, frame = -4)
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

#' Expand colon notation in values
#' E.g. "1, 3:5, 7" -> c(1, 3, 4, 5, 7)
#' @keywords internal
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

#' Check and coerce class of x to match type
#' @keywords internal
.check_coerce_class <- function(x, type, .format_date, item_name) {

  pre_NA <- sum(is.na(x))

  if (type == "double" && !is.double(x)) {
    add_message(
      "'type' defintion is 'double' but variable is of class '",
      paste0(class(x), collapse = ", "), "' --> coerced to 'double'.",
      frame = -2
    )
    x[] <- suppressWarnings(as.double(x))
  }

  if (type == "integer" && any(x[!is.na(x)] %% 1 != 0)) {
    add_message(
      "'type' defintion is 'integer' but variable is of class '",
      paste0(class(x), collapse = ", "), "' --> coerced to 'integer'.",
      frame = -2
    )
    x[] <- suppressWarnings(as.integer(x))
  }

  if (type == "numeric" && !is.numeric(x)) {
    add_message(
      "'type' defintion is 'numeric' but variable is of class '",
      paste0(class(x), collapse = ", "), "' --> coerced to 'numeric'.",
      frame = -2
    )
    x[] <- suppressWarnings(as.numeric(x))
  }

  if (type == "character" && !is.character(x)) {
    add_message(
      "'type' defintion is 'character' but variable is of class '",
      paste0(class(x), collapse = ", "), "' --> coerced to 'character'.",
      frame = -2
    )
    x[] <- suppressWarnings(as.character(x))
  }

  if (type == "logical" && !is.logical(x)) {
    add_message(
      "'type' defintion is 'logical' but variable is of class '",
      paste0(class(x), collapse = ", "), "' --> coerced to 'logical'.",
      frame = -2
    )
    x[] <- suppressWarnings(as.logical(x))
  }

  if (type == "date" && is.character(x)) {
    add_message(
      "'type' defintion is 'date' but variable is of class 'character' ",
      "--> coerced to 'date' with format ", .format_date,
      frame = -2
    )
    x <- suppressWarnings(as.Date(x, format = .format_date))
  }

  if (type == "date" &&
      !inherits(x, c("Date", "POSIXct", "POSIXt", "character"))) {
    add_message(
      "'type' defintion is 'date' but variable is of class '",
      paste0(class(x), collapse = ", "), "' --> coerced to 'character'.",
      frame = -2
    )
    x[] <- suppressWarnings(as.character(x))
  }
  post_NA <- sum(is.na(x))
  if (post_NA > pre_NA) {
    add_message(
      "Coercion to '", type, "' introduced ", post_NA - pre_NA,
      " additional NA values in item '", item_name, "'.",
      frame = -2
    )
  }

  x
}

#' Check and estimate type if missing
#' @keywords internal
.check_type <- function(type, x) {
  # type estimation --------
  if (!has_info(type)) {
    if (is.numeric(x)) type <- "numeric"
    #if (is.integer(x)) type <- "integer"
    #if (is.double(x)) type <- "numeric"
    if (is.character(x)) type <- "character"
    if (is.factor(x)) type <- "factor"
    if (is.logical(x)) type <- "logical"
    if (inherits(x, c("Date", "POSIXct", "POSIXt"))) type <- "date"
    add_message("Type is missing and is estimated as ", "'", type, "'.", frame = -3)
  }

  # type check --------

  if (type %in% c("real", "float", "numeric")) {
    type <- "numeric"
    add_message("Type 'real' or 'float' is replaced by 'numeric'.", detail = 2, frame = -3)
  }

  type
}

#' Build and validate dic attribute list
#'
#' @param x Optional. Used for type estimation only if `type` missing.
#' @param validate If FALSE: minimal normalization only (no parsing/checks).
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
                                  ...,
                                  validate = TRUE) {

  dots <- list(...)

  # Minimal normalization for prototypes
  if (!validate) {
    out <- c(
      list(
        item_name = item_name,
        item_label = item_label,
        values = values,
        value_labels = value_labels,
        missing = missing,
        weight = weight,
        type = type,
        class = class
      ),
      dots
    )
    if (!is.null(recodes)) out$recodes <- recodes
    return(out)
  }

  # ---- replicate your old "missing -> default" logic ----
  if (!has_info(item_name)) {
    # Caller may set this later; but old behavior used symbol name.
    # Here: leave to factory dic() to set from match.call() if desired.
    add_message("'item_name' definition is missing.", frame = -2)
    item_name <- NA_character_
  }

  if (!has_info(item_label)) {
    add_message("'item_label' definition is missing and copied from item_name.", frame = -2)
    item_label <- item_name
  }

  if (!has_info(weight)) {
    weight <- 1
    add_message("'weight' definition is missing and set to 1.", frame = -2)
  }

  if (!has_info(class)) {
    class <- "item"
    add_message("'class' definition is missing and set to 'item'.", frame = -2)
  }

  # type estimation/check (needs x)
  if (is.null(x)) {
    # if no x provided, we can’t estimate; keep as provided
    if (!has_info(type)) {
      type <- NA_character_
      add_message("Type is missing and cannot be estimated (x is NULL).", frame = -2)
    }
  } else {
    type <- .check_type(type, x)
  }

  # parse values/missing/value_labels based on type
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
        add_message(
          "'values' definition for item '", item_name,
          "' is missing and taken from 'value_labels' definition.", frame = -2
        )
      }

      # apply names(values) from value_labels to values_parsed
      if (has_info(values_parsed) && has_info(value_labels_parsed[[1]])) {
        .id <- sapply(value_labels_parsed[[1]], function(v) {
          id <- which(as.character(v) == as.character(values_parsed))
          if (length(id) == 0) {
            add_message(
              "Value from 'value_labels' not found in 'values' for item '",
              item_name, "'.", frame = -4
            )
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

  # parse recodes
  recodes_parsed <- NULL
  if (has_info(recodes)) recodes_parsed <- .extract_scores(recodes)

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


