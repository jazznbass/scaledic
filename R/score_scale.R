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
#' each scale defined in that list. The default function for calculating the
#' scale score is "mean", a weighted mean function.
#'
#' @param data A data frame.
#' @param filter A logical expression for any *dic* attribute (e.g. `scale ==
#'   "ITRF" & subscale == "Int"`).
#' @param scales Alternatively, a named list with scale definition created with
#'   the `get_scales()` function. Not used when `filter` is provided.
#' @param fun A function or a string with the name of a predefined function that
#'   is applied to calculate the scale score. The function should take a
#'   data.frame of item values and a numeric vector of weights as arguments. The
#'   function should return a single numeric value representing the calculated
#'   score. Predefined functions are "mean" (weighted mean), "sum" (weighted
#'   sum), "fa_score" (factor scores based on a factor analysis with one
#'   factor), and "pc_score" (scores based on the first principal component).
#'
#' @param min_valid Minimal number of valid values of each case. The score of a
#'   case will be set to NA if the number of valid values is below this
#'   threshold. A value between 0 and 1 indicates a proportion of values (e.g.,
#'   0.5 = 50 percent at least have to be valid).
#' @param max_na Maximum number of NA values of each case. The score of a case
#'   will be set to NA if the number of valid values is above this threshold. A
#'   value between 0 and 1 indicates a proportion of values (e.g., 0.5 = 50
#'   percent at most NAs are allowed).
#' @param label A character string with a label for the resulting score
#'   variable. Automatically generated if label is not set.
#' @param sum (Deprecated) If `FALSE`, a weighted mean function is applied for
#'   building the scores. If `TRUE`, a weighted sum function is applied. When
#'   argument fun is set, `sum` is ignored.
#' @param var_recoding Name if the *dic* attribute that may contain recoding
#'   information. Defaults to `scores`.
#' @param use_weights If `TRUE`, weights are applied when calculating the
#'   scores.
#' @return A data frame with the calculated scale score. If only the filter
#'   argument is provided, a vector with the calculated scale score is returned.
#' @seealso \code{\link{get_scales}}, \code{\link{apply_dic}},
#'   \code{\link{dic_attr}}
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
                        var_recoding = "recodes",
                        use_weights = TRUE) {

  filter <- substitute(filter)

  return_vector <- FALSE # indicates if a vector or a data frame is returned.

  if (is.null(filter) && is.null(scales)) {
    abort("Either 'filter' or 'scales' argument must be provided.")
  }

  if (!is.null(filter) && !is.null(scales)) {
    notify("Both 'filter' and 'scales' arguments are provided. 'filter' argument is used.")
  }

  # if filter is provided, create a scales list with one element based on the filter.

  if (!is.null(filter)) {
    return_vector <- TRUE
    scales <- list(NA)
    if(!is.null(label)) names(scales) <- label
    attr(scales[[1]], "filter") <- substitute(filter)
  }

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
      var_recoding = var_recoding,
      use_weights = use_weights,
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
                         var_recoding = "recodes",
                         use_weights = TRUE,
                         function_name) {


  if(!is.null(sum)) {
    if (identical(sum, TRUE)) fun <- "sum"
    if (identical(sum, FALSE)) fun <- "mean"
  }

  if (!is.null(label))
    notify("scoring '", label, "' with '", fun, "'", type = "h1")

  values <- NA

  if (is.character(fun)) {
    if (!fun %in% names(score_functions)) {
      abort("Unknown function name provided in 'fun' argument. Predefined functions are: ",
           paste(names(score_functions), collapse = ", "))
    }
    function_name <- fun #deparse(match.call()$fun)
    fun <- score_functions[[fun]]
  }

  # select cols -------
  df <- data[, get_index_from_dic(data = data, filter, class = "item")]

  # Check if all variables are numeric -----
  if (!all(sapply(df, is.numeric))) {
    abort("Not all variables are numeric.")
  }

  # id of cases with enough valid values ----
  id_valid <- .id_valid(df, min_valid, max_na)

  # recode items -----
  df <- .recode_dic_items(df, var_recoding)$df

  # reverse items ----
  if (use_weights) df <- .reverse_items(df)

  # extract weights --------
  if (use_weights) {
    weights <- sapply(df, function(x) {
      weight <- dic_attr(x, "weight")
      if (is.null(weight)) {
        weight <- 1
        notify("Weight information missing. Set weight to 1.")
      }
      weight
    })
    weights <- abs(weights)
  } else {
    weights <- rep(1, length(weights))
  }

  # calculate scores ----
  new_score <- rep(NA, nrow(df))
  new_score[id_valid] <- do.call(fun, list(df[id_valid, ], weights)) |> unname()

  # remove cases with too many NAs ----
  invalid <- nrow(df) - length(id_valid)
  if (invalid > 0) {
    notify("Set NA for ", invalid, " cases due to too many missing values.")
  }

  # set all values outside of id_valid to NA
  new_score[-id_valid] <- NA

  notify("Scores calculated for ", sum(!is.na(new_score)), " cases (",
         round(sum(!is.na(new_score))/length(new_score) * 100, 1) ,
         "%)", type = "i")

  # set dictionary attributes ---
  class(new_score) <- c("dic", class(new_score))
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
    m <- apply(df, 1, function(x) weighted.mean(x, weights, na.rm = TRUE))
    na_cases <- apply(df, 1, function(x) any(is.na(x)))
    if (any(na_cases)) {
      notify(
        "Scores for ", sum(na_cases), " cases with missing values ",
        "are scaled to the full number of items.", type = "!"
      )
    }
    m * ncol(df)
  },
  "mean" = function(df, weights) {
    apply(df, 1, function(x) weighted.mean(x, weights, na.rm = TRUE))
  },
  "fa_score" = function(df, weights) {
    m <- as.matrix(df)
    # impute missing values with column means
    col_means <- colMeans(m, na.rm = TRUE)
    idx <- is.na(m)
    m[idx] <- col_means[col(m)[idx]]
    # perform factor analysis on the imputed data
    fa <- stats::factanal(m, factors = 1, scores = "regression", method = "ML")
    # return factor scores
    as.numeric(fa$scores)
  },
  "pc_score" = function(df, weights) {
    m <- as.matrix(df)
    # impute missing values with column means
    col_means <- colMeans(m, na.rm = TRUE)
    idx <- is.na(m)
    m[idx] <- col_means[col(m)[idx]]
    m <- scale(m)
    pc <- stats::prcomp(m, center = TRUE, scale. = TRUE)

    scores <- as.numeric(pc$x[, 1]) |> scale()
    if (cor(scores, rowMeans(m)) < 0) scores <- -scores

    scores
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


.id_valid <- function(data, min_valid, max_na) {

  if (isTRUE(min_valid < 1) && isTRUE(min_valid > 0)) {
    min_valid <- ceiling(min_valid * ncol(data))
    notify("min_valid interpreted as proportion and set to ", min_valid, " values")
  }
  if (isTRUE(max_na < 1) && isTRUE(max_na > 0)) {
    max_na <- trunc(max_na * ncol(data))
    notify("max_na interpreted as proportion and set to ", max_na, " values")

  }

  id_valid <- apply(data, 1, function(x)
    if(isTRUE(sum(!is.na(x)) < min_valid) || isTRUE(sum(is.na(x)) > max_na)) {
      FALSE
    } else {
      TRUE
    }
  )

  which(id_valid)
}

.reverse_items <- function(data) {

  signs <- sapply(data, function(x) {
    sign(dic_attr(x, "weight"))
  })

  max_values <- sapply(data, function(.x) max(dic_attr(.x, "values")))
  min_values <- sapply(data, function(.x) min(dic_attr(.x, "values")))

  # reverse values if negative weight is provide ----
  invalid <- sum(is.na(max_values) & any(signs == -1))
  if (invalid > 0) {
    abort(
      "Mising max values for ", invalid, " items with negative weights.\n",
      "In order to reverse items with negative weights, max and min values must be provided as dic attributes.",
    )
  }

  for (i_col in seq_along(data)) {
    if (signs[i_col] == -1) {
      data[[i_col]] <- max_values[i_col] - data[[i_col]] + min_values[i_col]
    }
  }

  if (sum(signs == -1) > 0)
    notify("Reversed ", sum(signs == -1), " items with negative weights.")

  data
}


