#' Score scale
#' Calculates the scale scores.
#'
#' @inheritParams get_index
#' @param bind If set TRUE, returns the complete data frame. If set false,
#'  returns a data frame with the new score variables for each scale.
#' @param sum If TRUE, function mean(x, na.rm = TRUE) is applied for buiding the scores. If FALSE, function sum(x, na.rm = TRUE) is applied. When argument FUN is set, `sum` is ignored.
#' @param label A character string with a label for the resulting score variable. Automatically generated if label is not set.
#' @param FUN A function for caluclating the score (e.g., median)
#' @param ... Further arguments passed to the FUN function (e.g., na.rm = TRUE)
#' @return A data frame
#' @export

score_scale <- function(data, filter,
                        scale = NULL, subscale = NULL, subscale_2 = NULL,
                        bind = FALSE,  sum = FALSE, label = NULL,
                        FUN = NULL, ...) {

  filter <- deparse(substitute(filter))
  if (!is.null(scale) || !is.null(subscale) || !is.null(subscale_2)) {
    filter <- .to_filter(scale = scale, subscale = subscale, subscale_2 = subscale_2)
  }

  .score_scale(data = data, filter = filter, sum = sum, label = label, FUN = FUN, bind = bind, ...)

}

.score_scale <- function(data, filter,
                        sum, label,
                        FUN, bind = bind, ...) {

  args <- list(...)

  function_name <- "score"
  values <- NA
  if(is.null(FUN)) {
    if (sum) {
      FUN <- base::sum
      args <- list(na.rm = TRUE)
      function_name <- "sum"
    }
    if (!sum) {
      FUN <- base::mean
      args <- list(na.rm = TRUE)
      function_name <- "mean"
    }
  }

  vars <- .get_index(data = data, filter, class = "item")

  df <- data %>% select(one_of(vars))

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
    score <- ifelse(sign == 1, x * weight, (max_values - x + min_values) * weight)
    score <- do.call(FUN, c(list(score), args))
    score
  })

  class(df) <- c("dic", class(df))


  if (!isFALSE(bind)) {
    df <- as.data.frame(df)
    names(df) <- bind
    df <- cbind(data, df)
  }

  ### set dictionary attributes
  if( is.null(label)) label <- "score"

  attr(df, .opt$dic) <- list()
  dic_attr(df, .opt$class) <- "score"
  dic_attr(df, .opt$score_filter) <- filter
  dic_attr(df, .opt$score_function) <- function_name
  dic_attr(df, .opt$scale) <- scale
  dic_attr(df, .opt$subscale) <- NA
  dic_attr(df, .opt$subscale_2) <- NA
  dic_attr(df, .opt$item_label) <- label
  dic_attr(df, .opt$item_name) <- label
  ###
  attr(df, "label") <- label
  df
}

tmp_old_score_scale <- function(data, filter,
                        scale = NULL, subscale = NULL, subscale_2 = NULL,
                        bind = FALSE,  sum = FALSE, label = NULL,
                        FUN = NULL, ...) {

  args <- list(...)

  filter <- deparse(substitute(filter))
  if (!is.null(scale) || !is.null(subscale) || !is.null(subscale_2)) {
    filter <- .to_filter(scale = scale, subscale = subscale, subscale_2 = subscale_2)
  }

  function_name <- "score"
  values <- NA
  if(is.null(FUN)) {
    if (sum) {
      FUN <- base::sum
      args <- list(na.rm = TRUE)
      function_name <- "sum"
    }
    if (!sum) {
      FUN <- base::mean
      args <- list(na.rm = TRUE)
      function_name <- "mean"
    }
  }

  vars <- .get_index(data = data, filter, class = "item")

  df <- data %>% select(one_of(vars))

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
    score <- ifelse(sign == 1, x * weight, (max_values - x + min_values) * weight)
    score <- do.call(FUN, c(list(score), args))
    score
  })

  class(df) <- c("dic", class(df))


  if (!isFALSE(bind)) {
    df <- as.data.frame(df)
    names(df) <- bind
    df <- cbind(data, df)
  }

  ### set dictionary attributes
  if( is.null(label)) label <- paste0(
    function_name,
    if (!is.null(scale)) paste0("_", scale),
    if (!is.null(subscale)) paste0("_", subscale),
    if (!is.null(subscale_2)) paste0("_", subscale_2)
  )
  attr(df, .opt$dic) <- list()
  dic_attr(df, .opt$class) <- "score"
  dic_attr(df, .opt$score_filter) <- filter
  dic_attr(df, .opt$score_function) <- function_name
  dic_attr(df, .opt$scale) <- scale
  dic_attr(df, .opt$subscale) <- subscale
  dic_attr(df, .opt$subscale_2) <- subscale_2
  dic_attr(df, .opt$item_label) <- label
  dic_attr(df, .opt$item_name) <- label
  ###
  attr(df, "label") <- label
  df
}

