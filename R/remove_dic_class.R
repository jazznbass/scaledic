#' Remove dictionary class and information from a data.frame or a vector
#'
#' @param data A vector or a data.frame (or an object that inherits from a data.frame)
#' @param remove_attributes If TRUE, all dictionary attributes are removed.
#'
#' @return An object not inheriting from class "dic".
#' @export
#' @examples
#' dat <- remove_dic_class(ex_itrf)
#' class(ex_itrf$itrf_I_1)
#' class(dat$itrf_I_1)
remove_dic_class <- function(data, remove_attributes = FALSE) {

  .remove_dic <- function(x) {
    if (inherits(x, "dic")) {
      class(x) <- class(x)[which(class(x) != "dic")]
      if (remove_attributes) dic_attr(x) <- NULL
    }
    x
  }

  if (inherits(data, "data.frame")) {
    for(i in 1:ncol(data)) {
      data[[i]] <- .remove_dic(data[[i]])
    }
  } else {
    data <- .remove_dic(data)
  }

  data
}


