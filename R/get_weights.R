


get_weights <- function(data, scale = NULL, subscale = NULL, subscale_2 = NULL, key = TRUE) {
  id <- get_index(data, scale = scale, subscale = subscale, subscale_2 = subscale_2)
  weights <- as.numeric(sapply(data[[id]], function(x) dic_attr(x, "weight")))
  if (key) {
    weights <- matrix(weights, ncol = 1, dimnames = list(id, "scale"))
  }
  weights
}
