#' Convert a data frame to a dictionary-like structure
#'
#' Converts each column of the input data frame into a 'dic' object, preserving
#' any existing labels as item labels.
#'
#' @param x A data frame to be converted.
#' @return A data frame with each column converted to a 'dic' object.
#' @export
as_dic <- function(x) {

  for (i_col in 1:ncol(x)) {
    if (is_dic(x[[i_col]])) next

    label <- if (!is.null(attr(x[[i_col]], "label"))) {
      attr(x[[i_col]], "label")
    } else {
      names(x)[i_col]
    }

    x[[i_col]] <- dic(
      x = x[[i_col]],
      item_name = names(x)[i_col],
      item_label = label
    )
  }
  x
}

#' Check if an object is of class 'dic'
#'
#' Checks if the provided object inherits from the 'dic' class.
#'
#' @param x An object to check.
#' @return TRUE if the object is of class 'dic', FALSE otherwise.
#' @export
is_dic <- function(x) {
  if (inherits(x, "dic") || !is.null(dic_attr(x))) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}


