#' Lookup normtable values
#'
#' Transforms raw values to norm values based on a normtable.
#'
#' @param rawscores A vector with rawscores.
#' @param group A vector with group affiliation.
#' @param normtable An excel file name or a dataframe containing a normtable.
#' @param from Label of the raw score variable in file.
#' @param to Label of the norm score variable in file.
#' @param group_label Label of the group variable in file.
#'
#' @return A vector with norm values
#' @export
#'
#' @examples
#' normtable <- data.frame(
#'   group = c(6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8),
#'   raw = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
#'   T = c(40, 42, 44, 46, 48, 50, 52, 54, 56, 58, 60, 37, 39, 41, 43, 45, 47,
#'         49, 51, 53, 55, 57)
#' )
#' rawscores <- c(5,5,3,1)
#' group <- c("6", "8", "6", "8")
#' lookup_norms(rawscores, group, normtable)
#'
#' ## When no group values are provided, raw scores are ambiguous:
#' lookup_norms(rawscores, normtable = normtable)

lookup_norms <- function(rawscores,
                         group,
                         normtable,
                         from = "raw",
                         to = "T",
                         group_label = "group") {

  if (inherits(normtable, "character")) {
    normtable <- .read_by_suffix(normtable)
  }

  if (missing(group)) group <- "..none"

  if (!from %in% names(normtable))
    stop(from, " was not found in normtable.")
  if (!to %in% names(normtable))
    stop(to, " was not found in normtable.")
  if (!from %in% names(normtable))
    stop(from, " was not found in normtable.")
  if (!group_label %in% names(normtable))
    stop(group_label, " was not found in normtable.")

  if (length(group) < length(rawscores))
    group <- rep(group, length = length(rawscores))

  lookup <- function(x, y) {

    if (identical(y, "..none")) {
      id <- which(normtable[[from]] == x)
      if (length(id) > 1) {
        warning("Multiple values found for raw ", x, ". NA returned.")
        return(NA)
      }
    } else {
      id <- which(normtable[[from]] == x & normtable[[group_label]] == y)
      if (length(id) > 1) {
        warning("Multiple values found for raw ", x, " and group ", y,
                ". NA returned.")
        return(NA)
      }
    }

    if (length(id) == 0) return(NA)
    if (length(id) > 1) {
      warning("Multiple values found for raw ", x,
              " and group ", y, " NA returned.")
      return(NA)
    }
    normtable[[to]][id]
  }

  out <- mapply(lookup, rawscores, group)
  out
}


