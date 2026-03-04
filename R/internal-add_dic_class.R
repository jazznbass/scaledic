#' Add dictionary class to a vector or data.frame
#'
#' Adds the class 'dic' to a vector or all variables within a vector if it
#' contains 'dic' attributes but does not yet inherit from class 'dic'.
#' This is useful when dic attributes have been added to a vector or data.frame
#' but the class 'dic' has been removed (e.g., by subsetting).
#'
#' If a data.frame is provided, all columns are checked for dic attributes and
#' the class 'dic' is added to those columns that have dic attributes but do not
#' yet inherit from class 'dic'.
#'
#' @param data A vector or a data.frame (or an object that inherits from a
#'   data.frame) containing dic attributes.
#' @return An object inheriting from class "dic". If a data.frame is provided,
#'  a data.frame with columns inheriting from class "dic".
#' @examples
#' item <- remove_dic_class(ex_itrf$itrf_I_1)
#' item
#' item |> add_dic_class()
#' @keywords internal
#' @export
add_dic_class <- function(data) {

  .add <- function(x) {
    if (!inherits(x, "dic")) {
      class(x) <- c("dic", class(x))
    }
    x
  }

  if (inherits(data, "data.frame")) {
    for(i in 1:ncol(data)) {
      if (!is.null(dic_attr(data[[i]]))) {
        data[[i]] <- .add(data[[i]])
      }
    }
  } else {
    if (!is.null(dic_attr(data))) {
      data <- .add(data)
    }
  }

  data
}
