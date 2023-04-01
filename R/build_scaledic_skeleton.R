#' Build a dictionary file template
#'
#' @param filename Character string. Default is 'dic_template.xlsx'
#' @param nrows Number of empty rows added to data frame.
#'
#' @return When 'filename' is not empty, it writes an Excel file with an empty
#'   template of a dic file. When 'filename' is NA, returns a data
#'   frame.
#'
#' @examples
#' build_scaledic_skeleton(NA, nrows = 3)
#' @export

build_scaledic_skeleton <- function(filename = "dic_template.xlsx", nrows = 0) {

  out <- matrix(NA, ncol = length(.dic_file) - 1, nrow = nrows)
  out <- as.data.frame(out)
  names(out) <- unlist(.dic_file[-1])

  if (is.na(filename)) return(out)
  openxlsx::write.xlsx(out, filename)
  cat(filename, " written at ", getwd(), "\n")
}
