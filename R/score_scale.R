#' Score scale calculates scale scores.
#'
#' @param data A data frame
#' @param filter A logical expression for any *dic* attribute (e.g. `scale ==
#'   "ITRF" & subscale == "Int"`)
#' @param sum If `FALSE`, a weighted mean function is applied for building the
#'   scores. If `TRUE`, a weighted sum function is applied. When argument fun is
#'   set, `sum` is ignored.
#' @param min_valid Minimal number of valid values that is required for
#'   calculating the mean. A value between 0 and 1 indicates a proportion of
#'   values (e.g., 0.5 = 50 percent of values have to be valid).
#' @param max_na Maximum number of NAs that are allowed before returning NA. A
#'   value between 0 and 1 indicates a proportion of values (e.g., 0.5 = 50
#'   percent NAs are allowed).
#' @param label A character string with a label for the resulting score
#'   variable. Automatically generated if label is not set.
#' @param fun A function for calculating the score (e.g., `weighted.median`).
#'   See details.
#' @param var_weight Name of the *dic* attribute that is applied to derive
#'   weights. Defaults to `weight`.
#' @details If you provide your own function, the first argument of that
#'   function must take the vector of values and the second argument the
#'   weights.
#' @return A data frame
#' @examples
#' dat <- apply_dic(ex_scaledic_data, ex_scaledic_dic)
#' # apply the default weighted mean function
#' score_scale(dat, scale == "rel", label = "Religious beliefs")
#' # apply the weighted sum function
#' score_scale(dat, scale == "rel", label = "Religious beliefs", sum = TRUE)
#' # provide an external function (here the weighted median function from the spatstat package)
#' score_scale(dat, scale == "rel", label = "Religious beliefs", fun = spatstat.geom::weighted.median)
#'
#' @export

score_scale <- function(data,
                        filter,
                        sum = FALSE,
                        min_valid = 1,
                        max_na = NA,
                        label = NULL,
                        fun = NULL,
                        var_weight = NULL) {

  filter <- deparse(substitute(filter))

  if (is.null(var_weight)) var_weight <- opt("weight")

  .score_scale(
    data = data,
    filter = filter,
    sum = sum,
    min_valid = min_valid,
    max_na = max_na,
    label = label,
    fun = fun,
    var_weight = var_weight,
    function_name = deparse(match.call()$fun)
  )

}

.score_scale <- function(data,
                         filter,
                         sum,
                         min_valid,
                         max_na,
                         label,
                         fun,
                         var_weight,
                         function_name) {

  msg <- c()

  values <- NA
  if(is.null(fun)) {
    if (sum) {
      fun <- .weighted_sum
      function_name <- "weighted sum"
    }
    if (!sum) {
      fun <- .weighted_mean
      function_name <- "weighted mean"
    }
  }

  df <- data[, .get_index(data = data, filter, class = "item")]

  if (isTRUE(min_valid < 1) && isTRUE(min_valid > 0)) {
    min_valid <- trunc(min_valid * nrow(df))
  }
  if (isTRUE(max_na < 1) && isTRUE(max_na > 0)) {
    max_na <- trunc(max_na * nrow(df))
  }

  # Check if all variables are numeric
  if (!all(sapply(df, is.numeric))) {
    stop("Not all variables are numeric.")
  }

  .get_weight <- function(x) {
    weight <- dic_attr(x, var_weight)
    if (is.null(weight)) {
      weight <- 1
      msg <<- c(msg, "Weight information missing. Set weight to 1.")
    }
    as.numeric(weight)
  }

  weight <- sapply(df, .get_weight)
  sign <- sign(weight)
  weight <- abs(weight)

  max_values <- sapply(df, function(.x) max(dic_attr(.x, "values")))
  min_values <- sapply(df, function(.x) min(dic_attr(.x, "values")))

 if (any(is.na(max_values)) && any(sign == -1)) {
  stop(
    "A negative weight is provided but max and min values of at least one item ",
    "are missing.\nNeed max and min values as dic attributes to reverse items."
  )
 }

  new_score <- apply(df, 1, function(x) {
    if(isTRUE(sum(!is.na(x)) < min_valid) || isTRUE(sum(is.na(x)) > max_na)) {
      return(NA)
    }
    score <- ifelse(sign == 1, x, max_values - x + min_values)
    do.call(fun, list(score, weight))
  })

  class(new_score) <- c("dic", class(new_score))

  ### set dictionary attributes
  if (is.null(label)) label <- "score"
  attr(new_score, opt("dic")) <- list()
  dic_attr(new_score, "class") <- "score"
  dic_attr(new_score, "score_filter") <- filter
  dic_attr(new_score, "score_function") <- function_name
  dic_attr(new_score, "type") <- "numeric"

  dic_attr(new_score, "values") <- c(
    min = do.call(fun, list(min_values, weight)),
    max = do.call(fun, list(max_values, weight))
  )

  dic_attr(new_score, "item_label") <- label
  dic_attr(new_score, "item_name") <- label
  attr(new_score, "label") <- label

  return_messages(msg)

  new_score
}


