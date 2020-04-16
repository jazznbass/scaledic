#' Table with descriptive statistics
#'
#' @param data A data frame
#' @param round Digits for round function
#'
#' @return A data frame with descriptive statistics
#' @export
descriptives <- function(data, round = 2, labels = FALSE) {

  out <- apply(data, 2, function(x)
    c(
      valid = sum(!is.na(x)),
      missing  = sum(is.na(x)),
      mean = mean(x, na.rm = TRUE),
      sd = sd(x, na.rm = TRUE),
      min = min(x, na.rm = TRUE),
      max = max(x, na.rm = TRUE),
      range = max(x, na.rm = TRUE) - min(x, na.rm = TRUE),
      median = median(x, na.rm = TRUE),
      mad = mad(x, na.rm = TRUE)
    ))

  out <- t(out)
  out <- round(out, round)
  out <- data.frame(out)
  out <- cbind(name = rownames(out), out)
  rownames(out) <- NULL
  if (labels) {
    lab <- c()
    for(i in 1: ncol(data)) {
      lab <- c(lab, dic_attr(data[[i]], .opt$item_label))
    }
    out$label <- lab#extract_dic(data)$item_label
    out <- out %>% select(name, label, everything())
  }
  out
}

