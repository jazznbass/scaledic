#' Rename items
#'
#' Rename items based on dic information.
#'
#' @param data A data frame
#' @param pattern A character string with the syntax of the glue function (see
#'   [glue()]).
#' @param max_chars,chars If not NULL, only the first n chars of the resulting
#'   label will be applied.
#' @return A renamed data frame
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
    if (is.null(attr(data[[col]], .opt$dic))) next
    new_label <- .glue_dic(attr(data[[col]], .opt$dic), pattern = pattern)
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

  #if (!is.null(dic_env$value_labels)) {
  #  dic_env$value_labels <- paste0(
  #    dic_env$value_labels$value,
  #    " = ",
  #    dic_env$value_labels$label,
  #    collapse = "; "
  #  )
  #} else {
  #  dic_env$value_labels <- NA
  #}

  glue(pattern, .envir = list2env(dic_env))
}

