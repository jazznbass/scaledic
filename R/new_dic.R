#' Low-level constructor for dic vectors
#'
#' Creates a dic vector with specified attributes.
#' Detailed parsing and validation is not performed by this function.
#' Use the `dic()` factory function for that purpose.
#'
#' Details:
#' - The function sets the class of `x` to include "dic".
#' - The provided `dic_attributes` list is assigned as the "dic" attribute.
#' - Optional haven-style variable label and value labels can be set.
#'
#' @param x An atomic vector (integer/double/logical/character/factor/Date, ...)
#' @param dic_attributes A named list with dic metadata.
#' @param label Optional haven-style variable label.
#' @param labels Optional haven-style value labels (named vector).
#' @return A dic vector with specified attributes.
#' @export
#' @keywords internal
new_dic <- function(x,
                    dic_attributes = list(),
                    label = NULL,
                    labels = NULL) {

  stopifnot(is.list(dic_attributes))
  dic_attributes <- utils::modifyList(dic_attributes_default(), dic_attributes)

  out <- structure(
    x,
    class = c("dic", class(x))
  )

  attr(out, opt("dic")) <- dic_attributes
  if (!is.null(label))  attr(out, "label")  <- label
  if (!is.null(labels)) attr(out, "labels") <- labels

  out
}



#' Default dic attributes
#' @keywords internal
dic_attributes_default <- function() {
  list(
    item_name = NA_character_,
    item_label = NA_character_,
    values = NULL,
    value_labels = NULL,
    missing = NULL,
    weight = 1,
    type = NA_character_,
    class = "item",
    recodes = NULL
  )
}

