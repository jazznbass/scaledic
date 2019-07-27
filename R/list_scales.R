#' List scales
#'
#' @param data The target data frame
#'
#' @return A list with scales on diferent levels
#' @export
list_scales <- function(data, labels = FALSE) {
  # out <- list()
  # out$scales <- unique(unlist(lapply(data, function(x) dic_attr(x, .opt$scale))))
  # out$subscales  <- unique(unlist(lapply(data, function(x)
  #  paste0(dic_attr(x, .opt$scale), "_", dic_attr(x, .opt$subscale)))))[-1]
  # out$subscales2 <- unique(unlist(lapply(data, function(x)
  #  paste0(dic_attr(x, .opt$scale), "_", dic_attr(x, .opt$subscale), "_", dic_attr(x, .opt$subscale_2)))))[-1]
  # out
  filter <- which(sapply(data, function(x) !is.null(attr(x, .opt$dic))))

  if(!labels) {
    out <- sapply(data[filter], function(x)
      cbind(
        dic_attr(x, .opt$scale),
        dic_attr(x, .opt$subscale),
        dic_attr(x, .opt$subscale_2)
      ))
  }
  if(labels) {
    out <- sapply(data[filter], function(x)
      cbind(
        dic_attr(x, .opt$scale_label),
        dic_attr(x, .opt$subscale_label),
        dic_attr(x, .opt$subscale_2_label)
      ))
  }
  out <- as.data.frame(unique(t(out)))
  names(out) <- c("Scale", "Subscale", "Subscale_2")
  out <- out[order(out[["Scale"]], out[["Subscale"]], out[["Subscale_2"]]), ]
  rownames(out) <- NULL
  out
}
