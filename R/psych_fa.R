#' Factor analyses based on the psych::fa function
#'
#' @param ... Arguments passed to the psych::fa function
#' @param sort If TRUE, loadings are sorted
#' @param cut loadings below cut will be ommited in the resulting data frame
#' @param round number of digitis to round loadings (based on the base::round function)
#' @param type If 'html' returns an html table. Otherwise a data.frame
#'
#' @return A data.frame
#' @export

psych_fa <- function(..., sort = TRUE, cut = 0.2, round = 2, type = "df") {
  args <- list(...)
  res <- do.call(psych::fa, args)
  if(sort) res <- psych::fa.sort(res)
  res <- loadings(res)
  res <- unclass(res)
  res <- round(res, round)
  res[res<cut] <- ""
  res <- as.data.frame(res)

  if (type == "html") {
    res <- kable(res)
    res <- kable_styling(res)
  }

  return(res)
}
