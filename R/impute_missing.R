#' Impute missing values
#'
#' Impute missing values in items of a data frame based on their dic attributes.
#' You can specify a filter to select which items to impute based on their dic
#' attributes. If force_to_scale is set to TRUE, the imputed values will be
#' rounded and constrained to the scale's minimum and maximum values.
#'
#' @details This function uses the Amelia (continuous) or mice (ordinal) package
#'   to impute missing values in items of a data frame. You can specify a filter
#'   to select which items to impute based on their dic attributes. If
#'   force_to_scale is set to TRUE, the imputed values will be rounded and
#'   constrained to the scale's minimum and maximum values.
#'
#' @param data A data frame.
#' @param filter A logical expression for any dic attribute (e.g. `scale ==
#'   "ITRF" & subscale == "Int"`).
#' @param method Method for imputation. Either "continuous" (default) or
#'   "ordinal". If "continuous", the Amelia package will be used for imputation.
#'   If "ordinal", the mice package will be used for imputation with ordinal
#'   logistic regression.
#' @param force_to_scale If TRUE, imputed values will be rounded and forced to
#'   the scale. That is, a value below the scale's minimum or maximum will be
#'   set to the scale's minimum and maximum. If FALSE, imputed values will not
#'   be adjusted.
#' @return A data frame with imputed data.
#' @export
impute_missing <- function(data,
                           filter = NULL,
                           method = "continuous",
                           force_to_scale = TRUE) {

  filter <- substitute(filter)

  if (!method %in% c("continuous", "ordinal")) {
    abort("Invalid method. Please choose either 'continuous' or 'ordinal'.")
  }
  id <- get_index_from_dic(data, filter, class = "item")
  if (!any(is.na(data[, id]))) {
    notify("No missing data. No imputation needed.")
    return(data)
  }

  if (method == "continuous") {
    cl <- class(data) # has problems with tibbles
    tmp <- utils::capture.output(imputations <- Amelia::amelia(
      as.data.frame(data[, id]),
      boot.type = "none",
      m = 1
    ))
    data[, id] <- imputations$imputations[[1]]
    class(data) <- cl
    notify("Applied 'Amelia' for single imputation with EM algorithm ",
           type = "h1")
    notify("Imputed ", imputations$missMatrix |> sum(), " values (",
           round(sum(imputations$missMatrix) /
           length(imputations$missMatrix) * 100, 1), "%)")
  }

  if (method == "ordinal") {

    notify("Applied 'mice' for single imputation ",
           "with ordinal logistic regression.", type = "h1")

    n_not_ordered <- sum(sapply(data[, id], function(x) !is.ordered(x)))
    if (n_not_ordered > 0) {
      warn(n_not_ordered, " items are not ordered factors. ",
        "They will be converted to ordered factors for imputation and ",
        "afterwards back to numeric.",
        "Please check the dic file for correct coding.")
    }
    data_ordinal <- lapply(data[, id], function(x) {
      if (!is.ordered(x)) {
        factor_by_label(x, ordered = TRUE)
      } else {
        x
      }
    }) |> as.data.frame()

    nlevels <- lapply(data_ordinal, function(x) {
      length(levels(x))
    }) |> unlist()

    if (!all(nlevels == nlevels[1])) {
      abort("All items must have the same number of levels for ordinal imputation.")
    }

    # single imputation with ordinal logistic regression (polr)
    imp <- mice::mice(
      data_ordinal,
      m = 1,
      method = "polr",
      maxit = 10,
      seed = 1,
      printFlag = FALSE
    )

    dat_imp <- mice::complete(imp, 1)
    offset <- dic_attr(data[[id[1]]], "values")[1] - 1
    if (offset != 0) {
      notify("Offset of ", offset, " applied to imputed values to match original coding.")
    }
    for(colum in id){
      data[[colum]][] <- as.numeric(dat_imp[[colum]]) + offset
    }
     notify("Imputed ", imp$nmis |> sum(), "values (",
            round(sum(imp$nmis) /
            (nrow(data_ordinal) * ncol(data_ordinal)) * 100, 1),
            "%)")
  }


  if (force_to_scale && method == "continuous") {
    forces <- 0
    for (i in id) {
      type <- dic_attr(data[[i]], opt("type"))
      values <- dic_attr(data[[i]], opt("values"))
      if (!(type %in% c("integer", "float", "real", "numeric", "double")) || is.null(values)) next
      x <- data[[i]]
      max_values <- max(values)
      min_values <- min(values)
      if (type %in% c("integer")) x[] <- round(x)
      forces <- forces + sum(x > max_values | x < min_values, na.rm = TRUE)
      x[] <- ifelse(x > max_values, max_values, x)
      x[] <- ifelse(x < min_values, min_values, x)
      data[, i] <- x
    }
    notify("Forced ", forces, " imputed values to scales' min and max values.")
  }
  data
}
