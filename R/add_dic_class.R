#' Add dictionary class to a vector
#'
#' Adds the class 'dic' to a vector or all variables within a vector if it contains 'dic' attributes.
#'
#' @param data A vector or a data.frame (or an object that inherits from a data.frame)
#'
#' @return An object inheriting from class "dic".
#' @examples
#' item <- remove_dic_class(ex_itrf$itrf_I_1)
#' item
#' item |> add_dic_class()
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
      if (!is.null(attr(data[[i]], "dic"))) {
        data[[i]] <- .add(data[[i]])
      }
    }
  } else {
    if (!is.null(attr(data, "dic"))) {
      data <- .add(data)
    }
  }

  data
}
