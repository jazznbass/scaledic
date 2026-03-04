#' Rename items based on dic information.
#'
#' This function renames the variables in a data frame based on the dic
#' attributes. The new names are created using the glue syntax (see [glue()])
#' and can include item name, item label, and other dic information.
#'
#' The default pattern is "\{item_label\}", which renames the variables to their
#' item labels. Other available dic attributes can be used in the pattern, such
#' as "item_name", "values", "value_labels", "weight", etc.
#' For example, the pattern "\{item_name\}: \{item_label\}" will rename the variables
#' to "item_name: item_label".
#'
#' If the resulting label exceeds `max_chars`, it will be truncated to the
#' specified length.
#'
#' @param data A data frame.
#' @param pattern A character string with the syntax of the glue function (see
#'   [glue()]). It can include any dic attribute enclosed in curly braces,
#'   e.g. "\{item_label\}", "\{item_name\}", "\{values\}", "\{weight\}", etc.
#' @param max_chars,chars If not NULL, only the first n chars of the resulting
#'   label will be applied.
#' @return A renamed data frame with variable names based on dic attributes.
#' @examples
#' ex_itrf  |>
#'   rename_items(pattern = "{reverse}{name}: {label}")  |>
#'   names()
#'
#' @export
rename_items <- function(data,
                         pattern = "{item_label}",
                         max_chars = NULL,
                         chars = max_chars) {

  # compatibility check with older version pre glue
  if (length(pattern) > 1 ||
      (length(pattern) == 1 && length(grep("\\{", pattern)) == 0)) {
    warning(
      "The pattern definition is deprecated. ",
      "Please use glue::glue style syntax to defnine pattern. ",
      "E.g. '{name}: {label}'"
    )
    return(
      rename_items_deprecated(
        data, pattern, chars = max_chars, char_weight = c("(-)", "(+)")
      )
    )
  }

  # end

  for (col in 1:ncol(data)) {
    if (is.null(attr(data[[col]], opt("dic")))) next
    new_label <- .glue_dic(attr(data[[col]], opt("dic")), pattern = pattern)
    if (length(new_label) != 0) {
      if (!is.null(max_chars)) new_label <- substring(new_label, 1, max_chars)
      names(data)[col] <- new_label
    }
  }
  data
}

.glue_dic <- function(dic_env, pattern, char_reverse = c("-", "+")) {

  dic_env$reverse <- ifelse(dic_env$weight < 0, "-", "+")
  if(inherits(dic_env$reverse, "logical")) dic_env$reverse <- ""
  dic_env$label <- dic_env$item_label
  dic_env$name <- dic_env$item_name
  dic_env$values <- paste0(dic_env$values, collapse = ",")

  glue(pattern, .envir = list2env(dic_env))
}

