#' Build a dictionary file template
#'
#' @param filename Character string. Default is 'dic_template.xlsx'
#'
#' @return Writes an Excel file with an empty template of a dic file.
#' @export

build_scaledic_skeleton <- function(filename = "dic_template.xlsx", nrows = 0) {
  out <- matrix(NA, ncol = length(.dic_file) - 1, nrow = nrows)
  out <- as.data.frame(out)
  names(out) <- unlist(.dic_file[-1])

  if (filename == "") return(out)
  openxlsx::write.xlsx(out, filename)
  cat(filename, " written at ", getwd(), "\n")
}

