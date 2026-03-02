#' Automatic scale scoring from a dic file
#'
#' This function is deprecated. Please score scales directly from within R. Use
#' the [get_scales()] and [score_scale()] functions. This function takes a
#' data frame and a dic file with scale definitions and applies the specified
#' scoring functions to create new variables for each scale. The scoring
#' functions can be specified in the dic file, and if not provided, it defaults
#' to using the mean of the items in the scale. The new variables will be named
#' according to the 'item_name' column in the dic file. The function first
#' checks for the presence of dic information either in the data frame's
#' attributes or as a separate argument. It then iterates through each scale
#' defined in the dic file, applies the specified scoring function to the
#' relevant items, and creates new variables for each scale. Finally, it returns
#' a data frame with the new scale scores.
#'
#' @param data A data frame with dic information. The dic information can be
#'   provided as an attribute of the data frame or as a separate argument.
#'
#' @param dic A dic file with scoring information. If NULL, the function will
#'   attempt to retrieve dic information from the data frame's attributes.
#'
#' @return A data frame with added scale scores. The new variables will be named
#'   according to the 'item_name' column in the dic file.
#' @keywords internal
#' @export
score_from_dic <- function(data,
                           dic = NULL) {



  warn("This function is deprecated. Please score scales directly from within R. ",
              "Use the `get_scales` and 'score_scales' functions.")

  if (is.null(dic)) dic <- attributes(data)$dic
  if (is.null(dic)) {
    notify("No dic information found.")
    return(NULL)
  }
  if (is.null(dic$scales)) {
    notify("No scale information found in dic.")
    return(NULL)
  }
  dic <- dic$scales

  new_var <- vector("list", length = nrow(dic))

  for (i in 1:nrow(dic)) {

    if (is.null(dic[[opt("score_function")]][i])) {
      dic[[opt("score_function")]][i] <- "mean"
      notify(
        "No scoring function specified for '", dic[[opt("item_name")]][i],
        "'. Defaulting to 'mean'."
      )
    }

    new_var[[i]] <- .score_scale(
      data,
      filter = dic[[opt("score_filter")]][i],
      label = dic[[opt("item_label")]][i],
      fun = dic[[opt("score_function")]][i],
      min_valid = 1,
      max_na = NA,
      var_weight = "weight"
    )
    names(new_var)[i] <- dic[[opt("item_name")]][i]
  }
  new_var <- as.data.frame(new_var)

  return(new_var)


}
