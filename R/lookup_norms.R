#' Look up norm table values
#'
#' Transforms raw values to norm values based on a norm table.
#'
#' @param rawscores A vector with raw scores.
#' @param group A vector with group affiliations or a list with vectors for
#'   multiple group categorizations.
#' @param normtable An excel file name or a data frame containing a norm table.
#' @param from Label of the raw score variable in file.
#' @param to Label of the norm score variable in file.
#' @param group_label Label of the group variable in file or a list with group
#'   labels for multiple group categorizations.
#'
#' @return A vector with norm values.
#' @export
#'
#' @examples
#' normtable <- data.frame(
#'   age = rep(c(6, 8, 6, 8), each = 11),
#'   gender = rep(c("m", "w"), each = 22),
#'   raw =  rep(0:10, 4),
#'   T = rep(c(40, 42, 44, 46, 48, 50, 52, 54, 56, 58, 60,
#'             37, 39, 41, 43, 45, 47, 49, 51, 53, 55, 57), 2) + rep(c(0,5), each = 22)
#' )
#' rawscores <- c(5,5,3,1)
#' group_age <- c("6", "8", "6", "8")
#' group_gender <- c("m", "m", "w", "w")
#'
#' lookup_norms(rawscores, group = list(age = group_age, gender = group_gender), normtable)
#'
#' ## When group values are not specified exactly, raw scores can be ambiguous:
#' lookup_norms(rawscores, group = list(gender = group_gender), normtable = normtable)
#'
#' lookup_norms(rawscores, normtable = normtable)
lookup_norms <- function(rawscores,
                         group = NULL,
                         normtable,
                         from = "raw",
                         to = "T",
                         group_label = names(group)) {

  msg <- c()

  if (inherits(normtable, "character")) {
    normtable <- .read_by_suffix(normtable)
  }

  if (!is.null(group)) {
    if (is.null(group_label)) group_label <- "group"
    if (!is.list(group)) group <- list(group)

    new_group_var <- paste(group_label, collapse = "#")
    normtable[[new_group_var]] <- do.call(
      paste,  c(normtable[, unlist(group_label), drop = FALSE], list(sep = "#"))
    )

    group_label <- new_group_var
    group <- do.call(paste,  c(group, list(sep = "#")))

    if (!all(group_label %in% names(normtable))) {
      stop(group_label, " was not found in normtable.")
    }
    if (length(group) < length(rawscores)) {
      if (length(group) == 1) {
        group <- rep(group, length = length(rawscores))
      } else {
        stop("Length of 'group' is smaller than length of 'rawscore'.")
      }
    }
  }

  if (!from %in% names(normtable)) {
    stop(from, " was not found in normtable.")
  }
  if (!to %in% names(normtable)) {
    stop(to, " was not found in normtable.")
  }

  lookup <- function(x, y) {
    if (is.na(y)) {
      id <- which(normtable[[from]] == x)
      if (length(id) > 1) {
        msg <<- c(msg, paste0(
          "Multiple values found for raw ", x,
          " (", paste0((normtable[[to]][id]), collapse = ", "), ")",
          ". NA returned."
        ))
        return(NA)
      }
    } else {
      id <- which(normtable[[from]] == x & normtable[[group_label]] == y)
      if (length(id) > 1) {
        msg <<- c(msg, paste0(
          "Multiple values found for raw ", x, " and group ", y,
          " (", paste0((normtable[[to]][id]), collapse = ", "), ")",
          ". NA returned."
        ))
        return(NA)
      }
    }

    if (length(id) == 0) return(NA)
    normtable[[to]][id]
  }

  if (is.null(group)) group <- NA
  out <- mapply(lookup, rawscores, group)

  if (length(msg) > 0) {
    warning(paste0(1:length(msg), ": ", msg, sep = "\n"), call. = FALSE)
  }

  out
}


