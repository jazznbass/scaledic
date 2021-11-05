#' Score scale
#' Calculates the scale scores.
#'
#' @param data A data frame
#' @param filter A logical expression for any dic attribute (e.g. scale == "ITRF" & subscale == "Int")
#' @param scale (deprecated) Character string.
#' @param subscale (deprecated) Character string.
#' @param subscale_2 (deprecated) Character string.
#' @param bind If set TRUE, returns the complete data frame. If set false,
#'  returns a data frame with the new score variables for each scale.
#' @param sum If TRUE, function mean(x, na.rm = TRUE) is applied for buiding the scores. If FALSE, function sum(x, na.rm = TRUE) is applied. When argument FUN is set, `sum` is ignored.
#' @param min_valid Minimal number of valid values that is required for calculating the mean.
#' A value between 0 and 1 indicates a proportion of values (e.g., 0.5 = 50 percent of values have to be valid).
#' @param max_na Maximum number of NAs that are allowed before returning NA.
#' A value between 0 and 1 indicates a proportion of values (e.g., 0.5 = 50 percent NAs are allowed).
#' @param label A character string with a label for the resulting score variable. Automatically generated if label is not set.
#' @param FUN A function for calculating the score (e.g., median)
#' @param ... Further arguments passed to the FUN function (e.g., na.rm = TRUE)
#' @return A data frame
#' @export

score_scale <- function(data, filter,
                        scale = NULL, subscale = NULL, subscale_2 = NULL,
                        bind = FALSE,  sum = FALSE, min_valid = 1, max_na = NA,
                        label = NULL,
                        FUN = NULL, ...) {

  filter <- deparse(substitute(filter))
  if (!is.null(scale) || !is.null(subscale) || !is.null(subscale_2)) {
    filter <- .to_filter(scale = scale, subscale = subscale, subscale_2 = subscale_2)
  }

  .score_scale(data = data, filter = filter, sum = sum, min_valid = min_valid, max_na = max_na, label = label, FUN = FUN, bind = bind, ...)

}

.score_scale <- function(data, filter,
                        sum,  min_valid = 1, max_na = NA, label,
                        FUN, bind = bind, ...) {

  args <- list(...)

  function_name <- "score"
  values <- NA
  if(is.null(FUN)) {
    if (sum) {
      FUN <- .sum
      args <- list(min_valid = min_valid, max_na = max_na)
      function_name <- "sum"
    }
    if (!sum) {
      FUN <- .weighted_mean
      args <- list(min_valid = min_valid, max_na = max_na)
      function_name <- "mean"
    }
  }

  vars <- .get_index(data = data, filter, class = "item")

  df <- data %>% select(all_of(vars))

  # Check if all variables are numeric
  .tmp <- map(df, ~any(c("numeric", "integer") %in% class(.))) %>% unlist()

  if (!all(.tmp)) {
    stop("Not all variables are numeric.")
  }

  weight <- df %>%
    map(~ dic_attr(.x, .opt$weight)) %>%
    unlist() %>%
    as.numeric()
  sign <- sign(weight)
  weight <- abs(weight)

  max_values <- df %>%
    map(~ max(dic_attr(.x, .opt$values))) %>%
    unlist() %>%
    as.numeric()
  min_values <- df %>%
    map(~ min(dic_attr(.x, .opt$values))) %>%
    unlist() %>%
    as.numeric()

  df <- apply(df, 1, function(x) {

    score <- ifelse(sign == 1, x, max_values - x + min_values)

    if (function_name == "sum") {
      score <- score * weight
      score <- do.call(FUN, c(list(score), args))
    }

    if (function_name == "mean") {
      score <- do.call(FUN, c(list(score, weight), args))
    }

    score
  })

  class(df) <- c("dic", class(df))


  if (!isFALSE(bind)) {
    df <- as.data.frame(df)
    names(df) <- bind
    df <- cbind(data, df)
  }

  ### set dictionary attributes
  if (is.null(label)) label <- "score"

  attr(df, .opt$dic) <- list()
  dic_attr(df, .opt$class) <- "score"
  dic_attr(df, .opt$score_filter) <- filter
  dic_attr(df, .opt$score_function) <- function_name
  dic_attr(df, .opt$type) <- "numeric"

  if (function_name == "sum")
    dic_attr(df, .opt$values) <- paste0(sum(min_values * weight), ":", sum(max_values * weight))

  if (function_name == "mean")
    dic_attr(df, .opt$values) <- paste0(mean(min_values), ":", mean(max_values))


  dic_attr(df, .opt$scale) <- NA
  dic_attr(df, .opt$subscale) <- NA
  dic_attr(df, .opt$subscale_2) <- NA
  dic_attr(df, .opt$item_label) <- label
  dic_attr(df, .opt$item_name) <- label
  ###
  attr(df, "label") <- label
  df
}


