#' Exploratory factor analysis based on the psych::fa function
#'
#' @param ... Arguments passed to the psych::fa function.
#' @param sort If TRUE, loadings are sorted.
#' @param cut Loadings below cut will be ommited in the resulting data frame.
#' @param round Number of digitis to round loadings (based on the base::round function)
#'
#' @return A data.frame
#' @export

exploratory_fa <- function(..., factor_names = NULL, sort = TRUE, cut = 0.2, round = 2) {
  args <- list(...)
  out <- do.call(psych::fa, args)
  if(sort) out <- psych::fa.sort(out)
  out <- loadings(out)
  out <- unclass(out)
  out <- round(out, round)
  out[out < cut] <- ""
  out <- as.data.frame(out)
  if (!is.null(factor_names)) names(out) <- factor_names[1:ncol(names)]

  out
}

