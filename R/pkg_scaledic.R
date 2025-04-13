#' Scale Dictionary
#'
#' A collection of procedures to bring labels, scales, missing values etc. to a data frame.
#'
#' @name scaledic-package
#' @aliases scaledic-package scaledic
#' @author Juergen Wilbert \[aut, cre\]
#' @keywords package
#' @importFrom openxlsx write.xlsx
#' @importFrom readxl read_excel read_xlsx
#' @importFrom vctrs vec_ptype2 vec_cast
#' @import purrr
#' @import Amelia
#' @import stats
#' @importFrom dplyr %>% relocate select full_join all_of rename as_tibble
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
#' library(dplyr)
#' dat  |>
#'   select_items(scale == "ITRF" & subscale == "Ext")  |>
#'   rename_items(pattern = "{subscale_2}:{name}")  |>
#'   summarise_all(mean, na.rm = TRUE)  |>
#'   round(2)  |>
#'   t()
"_PACKAGE"



