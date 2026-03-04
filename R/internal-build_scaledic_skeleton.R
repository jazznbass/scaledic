#' Build a dictionary file template for scale definition
#'
#' This function builds an empty skeleton of a dictionary file for scale
#' definition. It can either write an Excel file or return a data frame.
#'
#' @details The skeleton contains all necessary variable names for a dic file.
#' The user can specify the number of empty rows to be added to the data frame.
#' This is useful when creating a dic file for a data frame with many variables.
#' The function uses the variable names defined in the option 'dic_file_vars'.
#' These can be modified by the user if needed.
#' The function uses the 'openxlsx' package to write the Excel file.
#' Make sure to have it installed.
#'
#' @param filename Character string. Default is 'dic_template.xlsx'. If NA, returns
#'  a data frame instead of writing an Excel file.
#' @param nrows Number of empty rows added to data frame. Default is 0.
#' @return When 'filename' is not empty, it writes an Excel file with an empty
#'   template of a dic file. When 'filename' is NA, returns a data
#'   frame.
#' @examples
#' build_scaledic_skeleton(NA, nrows = 3)
#' @keywords internal
#' @export
build_scaledic_skeleton <- function(filename = NA, nrows = 0) {

  columns <- opt("dic_file_vars")
  out <- matrix(NA, ncol = length(columns), nrow = nrows,
                dimnames = list(NULL, columns))
  out <- as.data.frame(out)

  if (is.na(filename)) return(out)

  openxlsx::write.xlsx(out, filename)
  cat(filename, " written at ", getwd(), "\n")

}
