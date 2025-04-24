#' Enriches a data frame with dic information
#'
#' Adds dic information to all variables in a data frame that do not have a dic
#' attribute.
#' @param dat A data frame
#' @export
enrich_dic <- function(dat) {

  for (i in seq_along(dat)) {
    if (!is.null(attr(dat[[i]], opt("dic")))) next

    item_label <- attr(dat[[i]], "label")
    if (is.null(item_label)) item_label <- names(dat)[i]
    item_name <- names(dat)[i]
    dat[[i]] <- new_dic(
      dat[[i]],
      item_name = item_name,
      item_label = item_label
    )

  }

  dat

}

