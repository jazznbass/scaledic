#' Scale Dictionary
#'
#' A collection of procedures to bring labels, scales, missing values etc. to a data frame.
#'
#' @name scaledic-package
#' @aliases scaledic-package scaledic
#' @docType package
#' @author Juergen Wilbert [aut, cre]
#' @keywords package
#' @import purrr
#' @import Amelia
#' @import dplyr
#' @examples
#' # apply a dictionary file to a data frame
#' dat <- apply_dic(ITRF, dic_ITRF)
#' # check for typos (not allowed values)
#' dat <- check_values(dat, replace = NA)
#' # Single imputation (EM algorith from the Amelia package)
#' # based on the variables of the provided scale
#' dat <- impute_missing(dat, scale = "ITRF_Ext")
#' dat <- impute_missing(dat, scale = "ITRF_Int")
#' # Show a table with all scales and scale labels included in the data frame
#' list_scales(dat, labels = TRUE)
#' # Example with pipeline syntax. Would be much easier to use the "describe" function
#' # from the psch packages instead of summarise_all here.
#' dat %>%
#'   select(get_index(dat, scale = "ITRF", subscale = "Ext")) %>%
#'   names2item(short = TRUE, id = TRUE) %>%
#'   summarise_all(mean, na.rm = TRUE) %>%
#'   round(2) %>%
#'   t()
NULL


