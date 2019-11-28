#' Get index
#'
#' @param data A data frame
#' @param scale String with scale name
#' @param subscale String with subscale name
#' @param subscale_2 String with sub subscale name
#' @param names If names is TRUE, a vector with variable names will be returned.
#' Otherwise a vector with variable indices.
#'
#' @return A vector with indices
#' @export
get_index <- function(data, scale = NULL, subscale = NULL, subscale_2 = NULL, names = TRUE) {
  id <- 1:ncol(data)
  if (!is.null(scale)) {
    id <- which(data %>% map(~ dic_attr(.x, .opt$scale)) %in% scale)
  }
  if (!is.null(subscale)) {
    id.sub <- which(data %>% map(~ dic_attr(.x, .opt$subscale)) %in% subscale)
  }

  if (is.null(scale) && !is.null(subscale)) id <- id.sub
  if (!is.null(scale) && !is.null(subscale)) id <- intersect(id, id.sub)

  if (!is.null(subscale_2)) {
    id.sub2 <- which(data %>% map(~ dic_attr(.x, .opt$subscale_2)) %in% subscale_2)
    id <- intersect(id, id.sub2)
  }

  id <- id[which(data[id] %>% map(~ dic_attr(.x, .opt$class)) == "item")]

  if (names) id <- names(data)[id]
  id
}

# get_index <- function(data, scale, level, names = TRUE) {
#   if (missing(level)) level <- "scale"
#   attr <- .opt[[level]]
#   out <- which(data %>% map(~ dic_attr(.x, attr)) == scale)
#   if (names) out <- names(data)[out]
#   out
# }

