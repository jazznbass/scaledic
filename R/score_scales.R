#' Score scale
#' Calculates the scale scores.
#'
#' @inheritParams get_index
#' @param bind If set TRUE, returns the complete data frame. If set false,
#'  returns a data frame with the new score variables for each scale
#' @param FUN A function for caluclating the score (e.g., sum, mean, median)
#' @param ... Further arguments passed to the FUN function (e.g., na.rm = TRUE)
#' @return A data frame
#' @export

score_scale <- function(data, scale = NULL, subscale = NULL, subscale_2 = NULL, bind = FALSE, FUN = sum, ...) {

  vars <- get_index(data, scale, subscale, subscale_2)
  df <- data %>% select(one_of(vars))
  #weight <- as.numeric(unlist(data[, vars, drop = FALSE] %>% map(~ dic_attr(.x, .opt$weight))))
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
  #max_values <- as.numeric(unlist(data[, vars, drop = FALSE] %>% map(~ max(dic_attr(.x, .opt$values)))))
  #min_values <- as.numeric(unlist(data[, vars, drop = FALSE] %>% map(~ min(dic_attr(.x, .opt$values)))))

  df <- apply(df, 1, function(x) {
    score <- ifelse(sign == 1, x * weight, (max_values - x + min_values) * weight)
    score <- FUN(score, ...)
    score
  })
  if (!isFALSE(bind)) {
    df <- as.data.frame(df)
    names(df) <- bind
    df <- cbind(data, df)
  }

  ### set dictionary attributes
  attr(df, .opt$dic) <- list()
  dic_attr(df, .opt$class) <- "score"
  dic_attr(df, .opt$scale) <- scale
  dic_attr(df, .opt$subscale) <- subscale
  dic_attr(df, .opt$subscale_2) <- subscale_2
  dic_attr(df, .opt$item_label_short) <- paste(scale, subscale, subscale_2)
  dic_attr(df, .opt$item_label) <- paste(scale, subscale, subscale_2)
  ###

  df
}
