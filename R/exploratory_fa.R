#' Exploratory factor analysis based on the psych::fa function
#'
#' @param ... Arguments passed to the psych::fa function.
#' @param factor_names A character vector with names for the resulting factors. If not provided, default names are chosen.
#' @param sort If TRUE, loadings are sorted.
#' @param cut Loadings below cut will be ommited in the resulting data frame.
#' @param round Number of digitis to round loadings (based on the base::round function)
#'
#' @return A data.frame
#' @examples
#' exploratory_fa(
#'   select_items(ex_itrf, subscale %in% c('Int', 'Ext')),
#'   nfactors = 2
#' )
#' @export

exploratory_fa <- function(..., factor_names = NULL, sort = TRUE, cut = 0.2,
                           round = 2) {
  args <- list(...)
  #if (label) x <- rename_items(x)
  #args <- list(r = x, unlist(args))

  out <- do.call(psych::fa, args)
  var_exp <- out$Vaccounted
  if(sort) out <- psych::fa.sort(out)
  out <- loadings(out)
  out <- unclass(out)
  out <- round(out, round)
  out[abs(out) < cut] <- ""
  out <- as.data.frame(out)
  if (!is.null(factor_names)) names(out) <- factor_names[1:ncol(names)]
  out <- rbind(out, round(var_exp, round))
  out
}

