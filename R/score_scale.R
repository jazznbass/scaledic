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
#' The default function for calculating the scale score is "mean", a weighted
#' mean function.
#'
#' @param data A data frame.
#' @param filter A logical expression for any *dic* attribute (e.g. `scale ==
#'   "ITRF" & subscale == "Int"`).
#' @param scales Alternatively, a named list with scale definition created with
#'   the `get_scales()` function. Not used when `filter` is provided.
#' @param fun A function or a string with the name of a predefined function that
#'   is applied to calculate the scale score. The function should take a
#'   data.frame of item values and a numeric vector of weights as arguments.
#'   The function should return a single numeric value representing the
#'   calculated score. Predefined functions are "mean" (weighted mean),
#'   "sum" (weighted sum), and "fa_scores" (factor scores based on a factor
#'   analysis with one factor using the psych package).
#' @param min_valid Minimal number of valid values of each case. The score of a
#'   case will be set to NA if the number of valid values is below this
#'   threshold. A value between 0 and 1 indicates a proportion of
#'   values (e.g., 0.5 = 50 percent of values have to be valid).
#' @param max_na Maximum number of NA values of each case. The score of a
#'   case will be set to NA if the number of valid values is above this
#'   threshold. A value between 0 and 1 indicates a proportion of values
#'   (e.g., 0.5 = 50 percent NAs are allowed).
#' @param label A character string with a label for the resulting score
#'   variable. Automatically generated if label is not set.
#' @param sum If `FALSE`, a weighted mean function is applied for building the
#'   scores. If `TRUE`, a weighted sum function is applied. When argument fun is
#'   set, `sum` is ignored.
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
#' score_scale(dat, scale == "rel", label = "Religious beliefs", fun = "sum")
#' @author Jürgen Wilbert
#' @export

score_scale <- function(data,
                        filter = NULL,
                        scales = NULL,
                        fun = "mean",
                        min_valid = 1,
                        max_na = NA,
                        label = NULL,
                        sum = NULL,
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

  # if filter is provided, create a scales list with one element based on the filter.

  if (!is.null(filter)) {
    return_vector <- TRUE
    scales <- list(NA)
    if(!is.null(label)) names(scales) <- label
    attr(scales[[1]], "filter") <- substitute(filter)
  }

  if (is.null(var_weight)) var_weight <- opt("weight")

  new_scores <- list()
  for(i_scale in 1:length(scales)) {

    filter <- attr(scales[[i_scale]], "filter")
    label <- names(scales)[i_scale]

    new_scores[[i_scale]] <- .score_scale(
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
    names(new_scores)[i_scale] <- dic_attr(new_scores[[i_scale]], "item_label")
  }

  if(return_vector) {
    return(new_scores[[1]])
  } else {
    return(as.data.frame(new_scores, check.names = FALSE))
  }
}

.score_scale <- function(data,
                         filter,
                         fun,
                         min_valid,
                         max_na,
                         label,
                         sum = NULL,
                         var_weight,
                         var_recoding = "recodes",
                         function_name) {


  values <- NA

  if(is.null(fun)) {
    fun <- if (sum) "sum" else "mean"
  }

  if (is.character(fun)) {
    if (!fun %in% names(score_functions)) {
      stop("Unknown function name provided in 'fun' argument. Predefined functions are: ",
           paste(names(score_functions), collapse = ", "))
    }
    function_name <- fun #deparse(match.call()$fun)
    fun <- score_functions[[fun]]
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

  # determines invalid values based on min_valid and max_na and returns NA for those cases. For valid cases,

  id_valid <- apply(dat, 1, function(x)
    if(isTRUE(sum(!is.na(x)) < min_valid) || isTRUE(sum(is.na(x)) > max_na)) {
      FALSE
    } else {
      TRUE
    }
  )

  # reverse values if negative weight is provided.
  if (any(is.na(max_values)) && any(sign == -1)) {
    stop(
      "A negative weight is provided but max and min values of at least one item ",
      "are missing.\nNeed max and min values as dic attributes to reverse items."
    )
  }

  for (i_col in seq_along(df)) {
    if (sign[i_col] == -1) {
      df[[i_col]] <- max_values[i_col] - df[[i_col]] + min_values[i_col]
    }
  }

  #new_score <- apply(df, 1, function(x) do.call(fun, list(x, weight)))

  new_score <- do.call(fun, list(df, weight))
  new_score[!id_valid] <- NA

  class(new_score) <- c("dic", class(new_score))

  ### set dictionary attributes
  if (is.null(label)) label <- paste0("scale_", function_name)

  dic_attr(new_score) <- list(
    "item_name" = label,
    "item_label" = label,
    "type" = "numeric",
    "class" = "score",
    "score_filter" = filter,
    "score_function" = function_name
  )

  #dic_attr(new_score, "values") <- c(
  #  min = do.call(fun, list(min_values, weight)),
  #  max = do.call(fun, list(max_values, weight))
  #)

  attr(new_score, "label") <- label

  new_score
}


score_functions <- list(
  "sum" = function(df, weights) {
    apply(df, 1, function(x) sum(x * weights, na.rm = TRUE))
  },
  "mean" = function(df, weights) {
    apply(df, 1, function(x) weighted.mean(x, weights, na.rm = TRUE))
  },
  "fa_scores" = function(df, weights) {
    fa <- psych::fa(df, nfactors = 1)
    psych::factor.scores(df, fa, impute = "mean")$scores |> as.numeric()
  },
  "median" = function(df, weights) {
    weighted_median <- function(x, w) {
      o <- order(x)
      x <- x[o]
      w <- w[o]
      cw <- cumsum(w) / sum(w)
      x[which(cw >= 0.5)[1]]
    }
    apply(df, 1, function(x) weighted_median(x, weights))
  }




)






