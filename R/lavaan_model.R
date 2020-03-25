#' Extract a model definition that can be applied to lavaan::cfa for a confirmatory factor analysis
#'
#' @param scales A list with scale information
#' @param adds Additional model definitions
#'
#' @return A character string with a lavaan model definition
#' @export
lavaan_model <- function(scales, adds = "") {
  model <- lapply(scales, function(x) paste0(x, collapse = " + "))
  model <- paste0(names(model), " =~ ",  model, collapse = "\n")
  model <- paste0(model, "\n", adds, "\n")
  model
}

# confirmatory_fa <- function(data, scales, adds = "", ...) {
#   model <- lavaan_model(scales = scales, adds = adds)
#   out <- cfa(model = model, data = data, ...)
#   out
# }

# res <- dat %>%
#   scale() %>%
#   cfa(model = model, data = .)
#
# summary(res, fit.measures = TRUE)
#
