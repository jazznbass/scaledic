
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
    notify(
      "Found linebreaks in 'value_labels' definition and replaced them with '",
      char_split, "'"
    )
    value_labels <- gsub("\r\n", char_split, x = value_labels)
  }

  if (is.na(value_labels) || value_labels == "") return(NA)

  if (count_chars("=", value_labels) == 0) {
    notify("No equal sign found in value_labels. Entry is misspecified.",
           frame = -2)
    return(NA)
  }

  if (count_chars("=", value_labels) - count_chars(char_split, value_labels) != 1) {
    notify("value_labels entry is misspecified ",
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
    notify("value_labels entry is misspecified ",
           "(Did you forget to add a label after an = sign?).")
    return(NA)
  }

  if (type %in% c("integer", "numeric", "float", "double")) {
    id_na <- which(
      suppressWarnings(sapply(out[["value"]], function(y) is.na(as.numeric(y))))
    )
    if (length(id_na) > 0) {
      notify(
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
    notify("no entries found for values", msg_prefix)
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
      notify(type, " 'values' defintions need quotes -> added quotes")
      values <- sapply(values, function(x) paste0("'", x, "'"))
    }
    # Must be fully quoted, no embedded quotes of same type
    unquote_one <- function(p) {

      if (grepl("^'.*'$", p)) {
        inner <- substr(p, 2, nchar(p) - 1)
        if (grepl("'", inner, fixed = TRUE)) {
          notify("single-quoted values must not contain unescaped single ",
                 "quotes", msg_prefix)
          return(NA_character_)
        }
        return(inner)
      }
      if (grepl('^".*"$', p)) {
        inner <- substr(p, 2, nchar(p) - 1)
        if (grepl('"', inner, fixed = TRUE)) {
          notify("double-quoted values must not contain unescaped double ",
                 "quotes", msg_prefix)
          return(NA_character_)
        }
        return(inner)
      }
      notify(
        "Character values should be quoted with single or double quotes ",
        "-> added quotes", msg_prefix
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
    #    notify("Type 'double' expects exactly two numbers for min and max",
    #                msg_prefix)
    #    return(NULL)
    #  }
    #  values <- suppressWarnings(as.numeric(values))
    #
    #  if (values[1] > values[2]) {
    #    notify("min must be <= max", msg_prefix)
    #    return(NULL)
    #  }
    #}

  }

  if (any(is.na(values))) {
    notify("could not parse some values", msg_prefix)
    return(NULL)
  }

  # integer: all values must be integers
  if (type == "integer") {
    if (any(values %% 1 != 0)) {
      notify("type 'integer' cannot contain non-integer values", msg_prefix)
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
        notify("could not parse numeric value: ", s, msg_prefix)
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
        notify("invalid range token ", t, msg_prefix)
        return(NULL)
      }
      ab <- trimws(unlist(strsplit(t, ":", fixed = TRUE)))
      if (length(ab) != 2) {
        notify("invalid range token ", t, msg_prefix)
        return(NULL)
      }

      a <- parse_number(ab[1]); b <- parse_number(ab[2])
      if (any(is.na(c(a, b)))) {
        notify("range endpoints must be numeric: ", t, msg_prefix)
        return(NULL)
      }

      if (a %% 1 != 0 || b %% 1 != 0) {
        notify("range endpoints must be integers: ", t, msg_prefix)
        return(NULL)
      }

      a <- as.integer(a); b <- as.integer(b)
      if (a <= b) out <- c(out, seq.int(a, b)) else out <- c(out, seq.int(a, b, by = -1L))
      next
    }

    # single number
    n <- parse_number(t)
    if (is.na(n)) {
      notify("invalid numeric token: ", t, msg_prefix)
      return(NULL)
    }
    out <- c(out, n)
  }
  out
}



