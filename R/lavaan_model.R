#' Extract a model definition that can be applied to lavaan::cfa for a confirmatory factor analysis
#' based on provided scales and items.
#'
#' @details
#' - Each scale in `scales` is defined as a latent factor, with items
#'  as observed indicators.
#' - If `orthogonal = TRUE`, covariance parameters between latent factors are set to zero.
#' - Additional model definitions can be added via `adds`.
#'
#' @param scales A list with scale information. Each element in the list corresponds to a scale
#' and contains a character vector with item names.
#' @param adds Additional model definitions as a character string.
#' @param orthogonal IF TRUE, covariance parameters are set for orthogonal latent factors.
#'
#' @return A character string with a lavaan model definition.
#' @export
lavaan_model <- function(scales, adds = "", orthogonal = FALSE) {
  labels <-  gsub("[^A-Za-z0-9]", "_", names(scales))
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
