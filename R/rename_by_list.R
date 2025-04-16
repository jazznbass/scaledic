#' Rename variables based on a list
#'
#' Rename the variable in a dataset based of a provided renaming list
#'
#' @param data A data frame
#' @param file A filename of an Excel file or a data.frame containing at least
#'   two columns for renaming (variables to rename and target names)
#' @param to_from When a filename or a data.frame is provided, a named character
#'   with the names of the target  and source variable names (e.g., c("to" =
#'   "from")). When no filename is provided, to_from must be a vector with named
#'   variable names c("to1" = "from1", "to2" = "from2"))
#' @param to When a filename or a data.frame is provided, the name of the column
#'   with the target variable names. When no filename is provided, to must be a
#'   vector with target variable names
#' @param from When a filename or a data.frame is provided, the name of the
#'   column with the source variable names. When no filename is provided, from
#'   must a vector with source variable names
#'
#' @return A data frame
#' @keywords internal
#' @export
#' @examples
#' dat <- data.frame(A = NA, B = NA, C = NA, D = NA)
#' rename_by_list(dat, to = c("albert", "bea"), from = c("A", "B"))
#' rename_by_list(dat, to_from = c("carl" = "C", "daniel" = "D"))
#' dic <- data.frame(old = c("A", "B"), new = c("albert", "bea"))
#' rename_by_list(dat, dic, to_from = c("new" = "old"))
#' rename_by_list(dat, dic, to = "new", from = "old")
#' \dontrun{
#' rename_by_list(dat, "rename_list,xlsx", to_from = c("new" = "old"))
#' rename_by_list(dat, "rename_list,xlsx", to = "new", from = "old")
#' }
rename_by_list <- function(data,
                           file = NULL,
                           to_from = NULL,
                           to = NULL,
                           from = NULL) {
  if (!is.null(file)) {
    if (!inherits(file, "data.frame")) dic <- read_xlsx(file)
    if (inherits(file, "data.frame")) dic <- file
    if (!is.null(to_from)) {
      from <- dic[[to_from]]
      to <- dic[[names(to_from)]]
    } else {
      from <- dic[[from]]
      to <- dic[[to]]
    }
  }

  if (is.null(file) && !is.null(to_from)) {
    from <- to_from
    to <- names(to_from)
  }

  rn <- setNames(from, to)
  rn <- rn[rn %in% names(data)]
  rn <- rn[!is.na(names(rn))]

  if (length(rn) == 0) stop("No variables renamed")

  out <- rename(data, !!rn)

  cat(paste0(length(rn), " variables renamed\n"))

  out
}
