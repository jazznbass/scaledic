#' Score scale calculates scale scores
#'
#' Calculates scale scores based on item variables defined in the dictionary.
#' Uses a weighted mean (default) or weighted sum function to calculate the
#' scores. Allows for filtering items based on any *dic* attribute (e.g.,
#' `scale`, `subscale`, etc.). Also allows for providing a custom function for
#' calculating the scores.
#'
#' If the filter argument is provided, scale scores are calculated based on that
#' filter. If the scales argument is provided, scale scores are calculated for
#' each scale defined in that list.
#' The default function for calculating the scale score is a weighted mean function.
#'
#' If you provide your own function, the first argument of that function must
#' take the vector of values and the second argument the weights.
#' If you provide your own function, the first argument of that
#' function must take the vector of values and the second argument the
#' weights.
#'
#' @param data A data frame.
#' @param filter A logical expression for any *dic* attribute (e.g. `scale ==
#'   "ITRF" & subscale == "Int"`).
#' @param scales Alternatively, a named list with scale definition created with
#'   the `get_scales()` function. Not used when `filter` is provided.
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
#' @param var_recoding Name if the *dic* attribute that may contain recoding
#'   information. Defaults to `scores`.
#' @return A data frame with the calculated scale score. If only the filter argument
#'  is provided, a vector with the calculated scale score is returned.
#' @seealso \code{\link{get_scales}}, \code{\link{apply_dic}},
#'  \code{\link{dic_attr}}
#' @examples
#' dat <- apply_dic(ex_scaledic_data, ex_scaledic_dic)
#' # apply the default weighted mean function
#' score_scale(dat, scale == "rel", label = "Religious beliefs")
#' # apply the weighted sum function
#' score_scale(dat, scale == "rel", label = "Religious beliefs", sum = TRUE)
#' # provide an external function (here the weighted median function from the spatstat package)
#' #score_scale(dat, scale == "rel", label = "Religious beliefs", fun = spatstat.geom::weighted.median)
#' @author Jürgen Wilbert
#' @export

score_scale <- function(data,
                        filter = NULL,
                        scales = NULL,
                        sum = FALSE,
                        min_valid = 1,
                        max_na = NA,
                        label = NULL,
                        fun = NULL,
                        var_weight = NULL,
                        var_recoding = "recodes") {

  init_messages(); on.exit(print_messages())

  filter <- substitute(filter)

  return_vector <- FALSE # indicates if a vector or a data frame is returned.

  if (is.null(filter) && is.null(scales)) {
    stop("Either 'filter' or 'scales' argument must be provided.")
  }

  if (!is.null(filter) && !is.null(scales)) {
    add_warning("Both 'filter' and 'scales' arguments are provided. 'filter' argument is used.")
  }

  if (!is.null(filter)) {
    return_vector <- TRUE
    scales <- list(NA)
    names(scales) <- if(is.null(label)) "score" else label
    attr(scales[[1]], "filter") <- substitute(filter)
  }

  if (is.null(var_weight)) var_weight <- opt("weight")

  new_scores <- list()
  for(i_scale in 1:length(scales)) {

    filter <- attr(scales[[i_scale]], "filter")
    label <- names(scales)[i_scale]

    new_scores[[label]] <- .score_scale(
      data = data,
      filter = filter,
      sum = sum,
      min_valid = min_valid,
      max_na = max_na,
      label = label,
      fun = fun,
      var_weight = var_weight,
      var_recoding = var_recoding,
      function_name = deparse(match.call()$fun)
    )
  }

  if(return_vector) {
    return(new_scores[[1]])
  } else {
    return(as.data.frame(new_scores, check.names = FALSE))
  }
}

.score_scale <- function(data,
                         filter,
                         sum,
                         min_valid,
                         max_na,
                         label,
                         fun,
                         var_weight,
                         var_recoding = "recodes",
                         function_name) {


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

  df <- data[, get_index_from_dic(data = data, filter, class = "item")]

  # recode items

  tmp <- .recode_dic_items(df, var_recoding)
  df <- tmp$df

  # min max valid na

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
      add_message("Weight information missing. Set weight to 1.")
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

  #attr(new_score, opt("dic")) <- list()

  dic_attr(new_score) <- list()
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

  new_score
}


.weighted_mean <- function(x, weights) {
  weighted.mean(x, weights, na.rm = TRUE)
}

.weighted_sum <- function(x, weights) {
  sum(x * weights, na.rm = TRUE)
}


.mean <- function(x, min_valid, max_na) {

  if (isTRUE(min_valid < 1) && isTRUE(min_valid > 0)) min_valid <- trunc(min_valid * length(x))
  if(isTRUE(sum(!is.na(x)) < min_valid)) {
    return(NA)
  }

  if (isTRUE(max_na < 1) && isTRUE(max_na > 0))
    max_na <- trunc(max_na * length(x))
  if(isTRUE(sum(is.na(x)) > max_na)) {
    return(NA)
  }

  mean(x, na.rm = TRUE)

}


