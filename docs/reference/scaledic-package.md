# Scale Dictionary

A collection of procedures to bring labels, scales, missing values etc.
to a data frame.

## Author

Juergen Wilbert \[aut, cre\]

## Examples

``` r
# apply a dictionary file to a data frame
dat <- apply_dic(dat_itrf, dic_itrf)
#> ! (apply_dic)
#> 1: Dictionary file includes scale definitions.
#> ! (check_values)
#> 1: Replaced the following invalid values with NA:
#>   'itrf_I_1' is 7 at row 3192
#>   'itrf_I_2' is 9 at row 4651
#>   'itrf_I_13' is 4 at row 3699
#>   'itrf_I_20' is 4 at row 2799
#>   'itrf_E_4' is 6 at row 2621
#>   'itrf_E_6' is 9 at row 2599
#>   'itrf_E_7' is 9 at row 2599
#>   'itrf_E_8' is 9 at row 2599
#>   'itrf_E_9' is 9 at row 2599
#>   'itrf_E_10' is 9 at row 2599
#>   'itrf_E_11' is 9 at row 2599
#>   'itrf_E_12' is 9 at row 2599
#>   'itrf_E_13' is 9, 11 at rows 2599, 4146
#>   'itrf_E_14' is 9 at row 2599
#> ! (replace_missing)
#> 1: Replaced 15 missing values in 'itrf_E_16' with NA
#> 2: Replaced 15 missing values in 'itrf_I_10' with NA
#> 3: Replaced 15 missing values in 'itrf_I_19' with NA
#> 4: Replaced 15 missing values in 'itrf_I_9' with NA
#> 5: Replaced 160 missing values in 'itrf_E_3' with NA
#> 6: Replaced 18 missing values in 'itrf_E_8' with NA
#> 7: Replaced 18 missing values in 'itrf_I_2' with NA
#> 8: Replaced 18 missing values in 'itrf_I_4' with NA
#> 9: Replaced 19 missing values in 'itrf_E_1' with NA
#> 10: Replaced 19 missing values in 'itrf_E_10' with NA
#> 11: Replaced 19 missing values in 'itrf_E_9' with NA
#> 12: Replaced 19 missing values in 'itrf_I_11' with NA
#> 13: Replaced 19 missing values in 'itrf_I_14' with NA
#> 14: Replaced 19 missing values in 'itrf_I_17' with NA
#> 15: Replaced 20 missing values in 'itrf_E_7' with NA
#> 16: Replaced 21 missing values in 'itrf_E_11' with NA
#> 17: Replaced 21 missing values in 'itrf_I_12' with NA
#> 18: Replaced 21 missing values in 'itrf_I_13' with NA
#> 19: Replaced 21 missing values in 'itrf_I_15' with NA
#> 20: Replaced 21 missing values in 'itrf_I_16' with NA
#> 21: Replaced 21 missing values in 'itrf_I_5' with NA
#> 22: Replaced 22 missing values in 'itrf_E_12' with NA
#> 23: Replaced 22 missing values in 'itrf_E_2' with NA
#> 24: Replaced 22 missing values in 'itrf_I_20' with NA
#> 25: Replaced 22 missing values in 'itrf_I_6' with NA
#> 26: Replaced 23 missing values in 'itrf_E_13' with NA
#> 27: Replaced 23 missing values in 'itrf_I_8' with NA
#> 28: Replaced 24 missing values in 'itrf_E_6' with NA
#> 29: Replaced 25 missing values in 'itrf_I_7' with NA
#> 30: Replaced 27 missing values in 'itrf_E_15' with NA
#> 31: Replaced 29 missing values in 'itrf_I_24' with NA
#> 32: Replaced 35 missing values in 'itrf_I_23' with NA
#> 33: Replaced 47 missing values in 'itrf_E_5' with NA
#> 34: Replaced 52 missing values in 'itrf_E_14' with NA
#> 35: Replaced 528 missing values in 'itrf_I_1' with NA
#> 36: Replaced 73 missing values in 'itrf_E_4' with NA
#> ! (score_from_dic)
#> 1: Created 'itrf_ext' with scale definition 'scale == "ITRF" & subscale == "Ext"'.
#> 2: Created 'itrf_int' with scale definition 'scale == "ITRF" & subscale == "Int"'.
# check for typos (not allowed values)
dat <- check_values(dat, replace = NA)
#> ! (check_values)
#> 1: No errors found.
#> 
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
#> OPP:itrf_E_11 0.76
#> OPP:itrf_E_12 0.46
#> OPP:itrf_E_13 0.47
#> APD:itrf_E_14 0.38
#> APD:itrf_E_15 0.70
#> OPP:itrf_E_16 0.35
```
