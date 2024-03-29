% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/apply_dic.R
\name{apply_dic}
\alias{apply_dic}
\title{Apply dictionary}
\usage{
apply_dic(
  data,
  dic,
  factors = TRUE,
  set_label_attr = TRUE,
  coerce_class = TRUE,
  replace_missing = TRUE,
  score_scales = TRUE,
  check_values = FALSE,
  impute_values = FALSE,
  rename_var = NULL
)
}
\arguments{
\item{data}{Data frame}

\item{dic}{A data frame comprising a dictionary or a character string with a
filename (for now an Microsoft Excel file) containing a dictionary.}

\item{factors}{If set TRUE, variables defined as type \code{factor} in the dic
file will be turned into factors.}

\item{set_label_attr}{If TRUE, label attributes from the haven package are
set. These labels are shown in the Rstudio View panel.}

\item{coerce_class}{If set TRUE mismatches between class and dic type are
corrected.}

\item{replace_missing}{If TRUE, missing values from the dic are replaced with
NA}

\item{score_scales}{If TRUE and the dic files contains score scale
definitions these are applied}

\item{check_values}{If TRUE, performs the check_values function on the
variables of the data frame included in the dic file.}

\item{impute_values}{If TRUE and score_scales is TRUE, missing values are
automatically imputed based on scale information provided in the dic file.}

\item{rename_var}{When a character is provided, corresponding column from the
dic file is used to rename variables from rename_var to item_name.}
}
\value{
A data frame with dictionary information.
}
\description{
Joins a data frame with a dictionary file.
}
\examples{
dat <- apply_dic(dat_itrf, dic_itrf)
descriptives(dat)
}
