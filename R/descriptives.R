#' Table with descriptive statistics
#'
#' @param data A data frame
#' @param round Digits for round function
#' @param labels If TRUE, item labels are added.
#'
#' @return A data frame with descriptive statistics
#' @examples
#' descriptives(ex_itrf)
#' @export
descriptives <- function(data, round = 2, labels = FALSE) {

  .filter <- sapply(data, function(x) "numeric" %in% class(x))

  if (any(.filter)) {
    tmp <- .filter[!.filter]
    warning(
      "Some variables are not numeric and dropped from the analyzes: ",
      paste0(names(tmp), collapse = ", ")
    )
  }

  data <- data[, .filter]

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
    out$label <- lab
    out <- out %>% relocate(name, label)
  }
  out
}

