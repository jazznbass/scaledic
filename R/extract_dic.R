#' Extract a dictionary from a data file with dic information
#'
#' @param data A data frame with dic information.
#'
#' @return A data frame in a dictionary format.
#' @examples
#' extract_dic(ex_itrf)
#'
#' @export

extract_dic <- function(data) {

  id <- which_dic(data, items_only = TRUE)

  dic_names <- lapply(data[id], function(x) names(dic_attr(x)))
  dic_names <- unlist(dic_names)
  dic_names <- unique(dic_names)

  N <- length(id)

  out <- matrix(NA, nrow = N, ncol = length(dic_names))
  out <- as.data.frame(out)
  names(out) <- dic_names

  for (i_row in 1:N) {

    dic <- dic_attr(data[[id[i_row]]])
    .var <- !dic_names %in% c("value_labels", "values", "missing")
    for (var_col in dic_names[.var]) {
      if (is.null(dic[[var_col]]) || length(dic[[var_col]]) == 0) {
        out[i_row, var_col] <- NA
      } else {
        out[i_row, var_col] <- dic[[var_col]]
      }
    }

    # values to code
    values <- dic[[opt("values")]]

    #value_labels <- dic[[opt("value_labels")]]

    dic[[opt("type")]] <- tolower(dic[[opt("type")]])

    if (!has_info(values)) {
      values <- NA
    } else if (dic[[opt("type")]] %in% opt("numerics")) {
      u <- unique(diff(values))
      if (length(u) == 1 && u[1] == 1) {
        values <- paste0(min(values), ":", max(values))
      } else {
        values <- paste0(values, collapse = ", ")
      }

    } else if (dic[[opt("type")]] %in% c("factor", "character")) {
      values <- paste0("'", values,"'", collapse = ", ")
    } else {
      values <- paste0(values, collapse = ", ")
    }

    out[i_row, opt("values")] <- values

    values <- dic[[opt("values")]]

    if (has_info(names(values))) {
      .filter <- which(!is.na(names(values)))
      value_labels <- names(values)[.filter]
      if (length(.filter) > 0) {
        value_labels <- paste0(
          unname(values[.filter]), " = ", names(values)[.filter], collapse = "; "
        )
      } else {
        value_labels <- NA
      }
    } else {
      value_labels <- NA
    }

    out[i_row, opt("value_labels")] <- value_labels

    # missing to code

    missing <- dic[[opt("missing")]]
    if (has_info(missing)) {
      missing <- paste0(missing, collapse = ", ")
    } else {
      missing <- NA
    }

    out[i_row, opt("missing")] <- missing

  }

  order <- c(
    opt("item_name"), opt("item_label"), opt("values"), opt("value_labels"),
    opt("missing"), opt("weight")
  )



  order <- order[order %in% names(out)]

  out <- out[, c(order, names(out)[which(!names(out) %in% order)])]

  if (!is.null(attributes(data)$dic$scales)) {
    new_rows <- attributes(data)$dic$scales
    new_ids <- (nrow(out) + 1):(nrow(out) + nrow(new_rows))
    out[new_ids, names(new_rows)] <- new_rows
  }

  out
}
