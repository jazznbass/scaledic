#' List scales
#'
#' @param data The target data frame
#' @param levels Character vector with names of dic attributes used to extract scale information.
#' @param n_items If TRUE, number of items for each scale is shown
#' @param char_na Character for NAs.
#'
#' @return A data.frame with scales on different levels
#' @export
list_scales <- function(data,
                        levels = c("scale", "subscale", "subscale_2"),
                        n_items = FALSE,
                        char_na = "") {

  filter <- .get_dic_items(data)
  out <- data[, filter]
  out <- sapply(out, function(x)
    cbind(sapply(levels, function(y) dic_attr(x, y)))
  )
  out <- as.matrix(out)
  if (length(levels) > 1) out <- t(out)
  out <- as.data.frame(out)
  names(out) <- levels

  if (n_items) {
    n_scale <- list()
    for (i in 1:length(levels)) {
      n_scale[[i]] <- out %>%
        select(levels[i]) %>%
        table() %>%
        as.data.frame()
    }

    for (i in 1:length(levels)) {
      if (nrow(n_scale[[i]]) > 0) {
        by <- "."
        names(by) <- levels[i]
        rn <- "Freq"
        names(rn) <- paste0("n ", levels[i])
        out <- out %>%
          full_join(n_scale[[i]], by = by) %>%
          rename(!!!rn)
      }
    }
  }

  out <- unique(out)
  out <- out[, colSums(is.na(out)) != nrow(out)]
  out[] <- lapply(out, as.character)
  out[is.na(out)] <- char_na

  out <- out[order(out[[1]]),]

  out
}
