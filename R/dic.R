#' Dictionary class constructor
#'
#' @param x A variable
#' @param class default is "item"
#' @param ... further dic parameters
#'
#' @return An item of class dic
#' @export
#'
#' @examples
#' x <- dic(1:100, item_label = "The label of this item")
dic <- function(x, class = "item", type = "integer", ...) {
  class(x) <- c(.opt$dic, class(x))
  attr(x, .opt$dic) <- list(...)
  dic_attr(x, .opt$class) <- class
  dic_attr(x, .opt$type) <- type
  attr(x, "label") <- dic_attr(x, .opt$item_label)
  attr(x, "labels") <- dic_attr(x, .opt$values)
  x
}


