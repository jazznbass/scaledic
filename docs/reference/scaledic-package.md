# Scale Dictionary

A collection of procedures to bring labels, scales, missing values etc.
to a data frame.

## Author

Juergen Wilbert \[aut, cre\]

## Examples

``` r
# apply a dictionary file to a data frame
dat <- apply_dic(dat_itrf, dic_itrf)
#> 40 messages generated (type show_messages() to see details).
# check for typos (not allowed values)
dat <- check_values(dat, replace = NA)
#> 1 messages generated (type show_messages() to see details).
# Single imputation (EM algorith from the Amelia package)
# based on the variables of the provided scale
dat <- impute_missing(dat, scale == "ITRF" & subscale == "Ext")
#> -- Imputation 1 --
#> 
#>   1  2  3
#> 
dat <- impute_missing(dat, scale == "ITRF" & subscale == "Int")
#> Warning: There are observations in the data that are completely missing. 
#>          These observations will remain unimputed in the final datasets. 
#> -- Imputation 1 --
#> 
#>   1  2  3
#> 
# Show a table with all scales and scale labels included in the data frame
list_scales(dat, levels = c("scale_label", "subscale_label"))
#>                      scale_label subscale_label
#> 1 Integrated teacher report form  Internalizing
#> 2 Integrated teacher report form  Externalizing
# Example with pipeline syntax. Would be much easier to use the "describe" function
# from the psch packages instead of summarise_all here.
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
dat  |>
  select_items(scale == "ITRF" & subscale == "Ext")  |>
  rename_items(pattern = "{subscale_2}:{name}")  |>
  summarise_all(mean, na.rm = TRUE)  |>
  round(2)  |>
  t()
#>               [,1]
#> OPP:itrf_I_20 0.55
#> APD:itrf_E_1  0.95
#> APD:itrf_E_2  0.76
#> APD:itrf_E_3  0.57
#> APD:itrf_E_4  0.58
#> APD:itrf_E_5  0.93
#> APD:itrf_E_6  0.53
#> OPP:itrf_E_7  0.35
#> OPP:itrf_E_8  0.49
#> OPP:itrf_E_9  0.82
#> OPP:itrf_E_10 0.40
#> OPP:itrf_E_11 0.77
#> OPP:itrf_E_12 0.46
#> OPP:itrf_E_13 0.47
#> APD:itrf_E_14 0.38
#> APD:itrf_E_15 0.70
#> OPP:itrf_E_16 0.35
```
