#' List scales
#'
#' @param data The target data frame
#' @param labels If set TRUE, scale labels instead of abreviations are shown.
#'
#' @return A list with scales on diferent levels
#' @export
list_scales <- function(data, labels = FALSE) {

  #filter <- which(
  #  sapply(
  #    data, function(x) !is.null(attr(x, .opt$dic)) && dic_attr(x, .opt$class) == "item"
  #  )
  #)

  filter <- .get_dic_items(data)

  if(!labels) {
    out <- sapply(data[filter], function(x)
      cbind(
        dic_attr(x, .opt$scale),
        dic_attr(x, .opt$subscale),
        dic_attr(x, .opt$subscale_2)
      ))
    #return(list(out, filter))
    out <- as.data.frame(unique(t(out)))
    names(out) <- c("Scale", "Subscale", "Subscale_2")
  }

  if(labels) {
    out <- sapply(data[filter], function(x)
      cbind(
        dic_attr(x, .opt$scale),
        dic_attr(x, .opt$subscale),
        dic_attr(x, .opt$subscale_2),
        dic_attr(x, .opt$scale_label),
        dic_attr(x, .opt$subscale_label),
        dic_attr(x, .opt$subscale_2_label)
      ))
    out <- as.data.frame(unique(t(out)))
    names(out) <- c("Scale", "Subscale", "Subscale_2", "Label scale", "Label subscale", "Label subscale_2")

  }
  #out <- out[order(out[["Scale"]], out[["Subscale"]], out[["Subscale_2"]]), ]
  #rownames(out) <- NULL
  out %>%
    as_tibble() %>%
    arrange(Scale, Subscale, Subscale_2)

}
