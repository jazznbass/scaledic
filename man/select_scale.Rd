% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/select_scale.R
\name{select_scale}
\alias{select_scale}
\title{Select a scale
Selects a data frame with variables of a specific scale}
\usage{
select_scale(
  data,
  filter = NULL,
  scale = NULL,
  subscale = NULL,
  subscale_2 = NULL
)
}
\arguments{
\item{data}{A data frame with dic information}

\item{filter}{A logical expression for any dic attribute (e.g. scale == "ITRF" & subscale == "Int")}
}
\value{
A data frame
}
\description{
Select a scale
Selects a data frame with variables of a specific scale
}
