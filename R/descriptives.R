#' Table with descriptive statistics
#'
#' @param data A data frame
#' @param round Digits for round function
#' @param labels Deprecated. Use [rename_items()] instead.
#'
#' @return A data frame with descriptive statistics
#' @examples
#' descriptives(ex_itrf)
#' @export
descriptives <- function(data, round = 2, labels = FALSE) {

  on.exit(print_messages())

  .filter <- sapply(data, is.numeric)

  if (any(!.filter)) {
    add_message(
      "Some variables are not numeric and dropped from the analysis: ",
      paste0(names(.filter)[!.filter], collapse = ", ")
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
  if (labels) add_message("Deprecated. Use rename_items().")

  out
}

