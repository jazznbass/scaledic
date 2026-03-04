#' Scale Dictionary
#'
#' A collection of procedures to bring labels, scales, missing values etc. to a data frame.
#'
#' @name scaledic-package
#' @aliases scaledic-package scaledic
#' @author Juergen Wilbert \[aut, cre\]
#' @keywords package
#' @importFrom openxlsx write.xlsx
#' @import vctrs
#' @import stats
#' @importFrom glue glue
#' @importFrom tools file_ext
#' @examples
#' # apply a dictionary file to a data frame
#' dat <- apply_dic(dat_itrf, dic_itrf)
#' # check for typos (not allowed values)
#' dat <- check_values(dat, replace = NA)
#' # Single imputation (EM algorith from the Amelia package)
#' # based on the variables of the provided scale
#' dat <- impute_missing(dat, scale == "ITRF" & subscale == "Ext")
#' dat <- impute_missing(dat, scale == "ITRF" & subscale == "Int")
#' # Show a table with all scales and scale labels included in the data frame
#' list_scales(dat, levels = c("scale_label", "subscale_label"))
#' # Example with pipeline syntax. Would be much easier to use the "describe" function
#' # from the psch packages instead of summarise_all here.
#' dat  |>
#'   select_items(scale == "ITRF" & subscale == "Ext")  |>
#'   rename_items(pattern = "{subscale_2}:{name}")  |>
#'   lapply(function(x) fivenum(x)) |>
#'   as.data.frame()
"_PACKAGE"



