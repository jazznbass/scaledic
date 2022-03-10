#' Extract a model definition that can be applied to lavaan::cfa for a confirmatory factor analysis
#'
#' @param scales A list with scale information
#' @param adds Additional model definitions
#' @param orthogonal If FALSE adds code allowing for intercorrelated latent variables.
#'
#' @return A character string with a lavaan model definition
#' @export
lavaan_model <- function(scales, adds = "", orthogonal = FALSE) {
  labels <- names(scales)
  model <- lapply(scales, function(x) paste0(x, collapse = " + "))
  model <- paste0(labels, " =~ ",  model, collapse = "\n")
  model <- paste0(model, "\n", adds, "\n")

  if (orthogonal) {
    n <- length(labels)
    for(i in 1:(n - 1)) {
      .add <- paste0(labels[i], " ~~ ", paste0("0*", labels [(i + 1): n], collapse = " + "), "\n")
      model <- paste0(model, .add)
    }
  }

  if (!orthogonal) {
    n <- length(labels)
    for(i in 1:(n - 1)) {
        .add <- paste0(labels[i], " ~~ ", paste0(labels [(i + 1): n], collapse = " + "), "\n")
      model <- paste0(model, .add)
    }
  }
  model
}
