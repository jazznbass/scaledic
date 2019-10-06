#' List scales
#'
#' @param data The target data frame
#' @param labels If TRUE, scale labels instead of abreviations are shown.
#' @param n_items If TRUE, number of items for each scale, subscale, and sub_subscale is shown
#'
#' @return A list with scales on diferent levels
#' @export
list_scales <- function(data, labels = FALSE, n_items = FALSE) {

  filter <- .get_dic_items(data)

  if(!labels) {
    out <- data %>%
      select(filter) %>%
      sapply(function(x)
        cbind(
          dic_attr(x, .opt$scale),
          dic_attr(x, .opt$subscale),
          dic_attr(x, .opt$subscale_2)
        )) %>%
      t() %>%
      as.data.frame()
    names(out) <- c("Scale", "Subscale", "Subscale_2")
  }

  if(labels) {
    out <- data %>%
      select(filter) %>%
      sapply(function(x)
        cbind(
          dic_attr(x, .opt$scale),
          dic_attr(x, .opt$subscale),
          dic_attr(x, .opt$subscale_2),
          dic_attr(x, .opt$scale_label),
          dic_attr(x, .opt$subscale_label),
          dic_attr(x, .opt$subscale_2_label)
        )) %>%
      t() %>%
      as.data.frame()
    names(out) <- c("Scale", "Subscale", "Subscale_2", "Label scale", "Label subscale", "Label subscale 2")
  }

  if (n_items) {
    n_scale <- out %>%
      select(Scale) %>%
      table() %>%
      as.data.frame()
    n_subscale <- out %>%
      select(Subscale) %>%
      table() %>%
      as.data.frame()
    n_subscale2 <- out %>%
      select(Subscale_2) %>%
      table() %>%
      as.data.frame()

    out <- out %>%
      full_join(n_scale, by = c("Scale" = ".")) %>%
      full_join(n_subscale, by = c("Subscale" = ".")) %>%
      full_join(n_subscale2, by = c("Subscale_2" = ".")) %>%
      rename(
        "n Scale" = Freq.x,
        "n Subscale" = Freq.y,
        "n Subscale 2" = Freq
      )
  }


  out %>%
    unique() %>%
    as_tibble() %>%
    arrange(Scale, Subscale, Subscale_2)

}
