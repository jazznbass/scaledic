#' Add dictionary class to a data.frame or a vector
#'
#' @param data A vector or a data.frame (or an object that inherits from a data.frame)
#'
#' @return An object not inheriting from class "dic".
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
      data[[i]] <- .add(data[[i]])
    }
  } else {
    data <- .add(data)
  }

  data
}
