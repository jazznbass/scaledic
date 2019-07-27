get_reversed <- function(data, scale = NULL, subscale = NULL, subscale_2 = NULL) {
  id <- get_index(data, scale = scale, subscale = subscale, subscale_2 = subscale_2)
  weights <- as.numeric(sapply(data[[id]], function(x) dic_attr(x, "weight")))
  id[which(weights < 0)]
}
