#' Dictionary class low level constructor
#'
#' This is a low-level constructor for dic objects.
#'
#' Standard attributes are: `"item_name"`, `"item_label"`, `"weight"`, `"type"`,
#' `"values"`, `"value_labels"`, `"missing"`, `"recodes"`.
#'
#' @seealso `set_dic()`
#' @param x A variable
#' @param item_name Character
#' @param item_label Character
#' @param values Numeric or character vector with values. The vector can be
#'   named
#' @param value_labels Character of the form `value = label; value2 = label2`
#' @param missing Numeric or character vector with values
#' @param weight numeric
#' @param type defaults to data type of x
#' @param recodes Recoding information e.g. `4 = 1, .default = 0`
#' @param class default is "item"
#' @param ... further dic arguments (e.g. `source = "James (1891)"`)
#' @param .coerce_class Logical. If TRUE, tries to coerce classes of 'x' if
#'   class does not match the `type` argument
#' @param .format_date Optional string that is applied when character variable is
#'   coerced to a 'Date' class.
#' @return An item of class dic.
#' @export
#' @keywords internal
#' @examples
#' x <- new_dic(
#'   sample(c(1:5, -99), 20, replace = TRUE),
#'   item_name = "My item",
#'   item_label = "The label of this item",
#'   values = "1:5",
#'   value_labels = "1 = no; 2 = mhh; 3 = Okish; 4 = good; 5 = mighty",
#'   missing = "-99"
#' )
#' x
new_dic_old <- function(x,
                        item_name = NULL,
                        item_label = NULL,
                        values = NULL,
                        value_labels = NULL,
                        missing = NULL,
                        weight = 1,
                        type = NULL,
                        recodes = NULL,
                        class = "item",
                        ...,
                        .coerce_class = TRUE,
                        .format_date = "%Y-%m-%d") {


  init_messages(); on.exit(print_messages())

  # validate/ normalize missing attributes -----

  if (!has_info(item_name)) {
    item_name <- as.character(match.call()[2])
    add_message(
      "'item_name' definition is missing and set to '", item_name, "'."
    )
  }
  if (!has_info(item_label)) {
    add_message(
      "'item_label' definition is missing and copied from item_name."
    )
    item_label <- item_name
  }
  if (!has_info(weight)) {
    weight <- 1
    add_message("'weight' definition is missing and set to 1.")
  }
  if (!has_info(class)) {
    class <- "item"
    add_message("'class' definition is missing and set to 'item'.")
  }

  # check and estimate type ----
  type <- .check_type(type, x)

  # check and coerce class of x -----
  if (.coerce_class) {
    x <- .check_coerce_class(x, type, .format_date, item_name)
  }
  class(x) <- unique(c("dic", class(x)))

  # extract values, value_labels, missing  --------

  values <- .extract_values(values, type, item_name, field = "values")
  missing_values <- .extract_values(missing, type, item_name, field = "missing")

  if (has_info(value_labels)) {
    value_labels <- .extract_value_labels(value_labels, type)
  }

  if (has_info(value_labels)) {

    if (!has_info(values) && has_info(value_labels$value)) {
      values <- value_labels$value
      add_message("'values' defintion for item '", item_name,
                  "' is missing and taken from 'value_labels' definition.")
    }
    .id <- sapply(value_labels$value, function(x) {
      id <- which(as.character(x) == as.character(values))
      if (length(id) == 0) {
        add_message(
          "Value from 'value_labels' definition not found in 'values' ",
          "definition for item '", item_name, "'.", frame = -4
        )
        return(NA)
      }
      id
    })
    if (!any(is.na(.id))) names(values)[.id] <- trimws(value_labels$label)
  }

  # when type is factor, create factor -----
  if (type == "factor") x <- .set_factor(x)

  # extract recodes --------
  if (has_info(recodes)) {
    recodes <- .extract_scores(recodes)
  }

  # set list of dic-attributes ----
  dic_list <- list(
    item_name = item_name,
    item_label = item_label,
    values = values,
    value_labels = value_labels,
    missing = missing_values,
    weight = weight,
    type = type,
    class = class,
    ...
  )
  if (has_info(recodes)) dic_list$recodes <- recodes


  # dic_attr(x) <- dic_list

  # # add haven labels ----
  # attr(x, "label") <- dic_attr(x, "item_label")
  #if (has_info(values) && !is.null(names(values)))
  #attr(x, "labels") <- dic_attr(x, "values")
  x <- vctrs::new_vctr(
    x,
    dic = dic_list,
    class = "dic",
    inherit_base_type = TRUE
  )

  x
}
