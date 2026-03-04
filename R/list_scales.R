#' List scales in a data frame based on dic attributes
#'
#' Lists scales and subscales defined in the dic attributes of a data frame.
#' This is useful for getting an overview of the scales present in a dataset
#' that uses dic attributes to define scales.
#'
#' @details
#' The function extracts scale information from dic attributes of variables
#' in the data frame. By default, it looks for attributes named "scale",
#' "subscale", and "subscale_2". It can also count the number
#' of items for each scale or subscale if requested.
#'
#' @param data The target data frame.
#' @param levels Character vector with names of dic attributes used to extract
#'  scale information.
#' @param .n_items If TRUE, number of items for each scale is added in parentheses.
#' @param .char_na Character for NAs.
#' @param ... Additional dic attribute names to include in the output. Overrides
#'  the `levels` argument if provided.
#' @return A data.frame with scales on different levels.
#' @examples
#' ## List default scales
#' ex_itrf |> list_scales()
#'
#' ## List custom scale levels
#' ex_itrf |> list_scales("subscale_label", "subscale_2_label")
#' @export
list_scales <- function(data,
                        ...,
                        levels = NULL,
                        .n_items = FALSE,
                        .char_na = "") {

  dots <- c(...)

  if (length(dots) > 0) {
    if (!is.null(levels)) {
      warning("Both 'levels' and '...' provided. 'levels' will be ignored.")
    }
    levels <- c(dots)
  }
  if (is.null(levels)) {
    levels <- c("scale", "subscale", "subscale_2")
  }

  out <- lapply(data[, which_dic(data)], function(x) dic_attr(x)[levels] |> unlist())

  out <- do.call(rbind, out) |> as.data.frame()
  names(out) <- levels

  if (.n_items) {
    n_scale <- vector("list", length(levels))
    for (i in seq_along(levels)) {
      level_names <-  unique(out[[levels[i]]])
      for(x in level_names) {
        id <- which(out[[levels[i]]] == x)
        out[[levels[i]]][id] <- paste0(x, " (n=", length(id), ")")
      }
    }
  }

  # drop duplicate rows
  out <- unique(out)
  # drop columns with all NAs
  out <- out[, colSums(is.na(out)) != nrow(out), drop = FALSE]
  # convert to character
  out[] <- lapply(out, as.character)
  # replace NAs
  out[is.na(out)] <- .char_na
  # order by first column
  out <- out[order(out[[1]]), , drop = FALSE]
  # reset row names
  row.names(out) <- NULL

  out
}
