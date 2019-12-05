#' List scales
#'
#' @param data The target data frame
#' @param labels If TRUE, scale labels instead of abreviations are shown.
#' @param n_items If TRUE, number of items for each scale, subscale, and sub_subscale is shown
#' @param char_na Charcter for NA is.
#'
#' @return A data.frame with scales on different levels
#' @export
list_scales <- function(data, labels = FALSE, n_items = FALSE, char_na = "") {

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

    if (nrow(n_scale) > 0)
      out <- out %>%
        full_join(n_scale, by = c("Scale" = ".")) %>%
        rename("n Scale" = Freq)
    if (nrow(n_subscale) > 0)
      out <- out %>%
        full_join(n_subscale, by = c("Subscale" = ".")) %>%
        rename("n Subscale" = Freq)
    if (nrow(n_subscale2) > 0)
      out <- out %>%
        full_join(n_subscale2, by = c("Subscale_2" = ".")) %>%
        rename("n Subscale 2" = Freq)
  }

  out <- out %>%
    unique() %>%
    as_tibble() %>%
    arrange(Scale, Subscale, Subscale_2)

  out <- out[, colSums(is.na(out)) != nrow(out)]
  out[] <- lapply(out, as.character)
  out[is.na(out)] <- char_na

  out
}
