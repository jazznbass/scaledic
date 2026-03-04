# Scale Dictionary

A collection of procedures to bring labels, scales, missing values etc.
to a data frame.

## Author

Juergen Wilbert \[aut, cre\]

## Examples

``` r
# apply a dictionary file to a data frame
dat <- apply_dic(dat_itrf, dic_itrf)
#> ! Dictionary file includes scale definitions.
#> ! Scale information in dic files are not scored automatically (since 0.5.0). Use `score_from_dic()` to... [truncated]
#> ! Replaced 528 missing values in 'itrf_I_1' with NA
#> ! Replaced 18 missing values in 'itrf_I_2' with NA
#> ! Replaced 18 missing values in 'itrf_I_4' with NA
#> ! Replaced 21 missing values in 'itrf_I_5' with NA
#> ! Replaced 22 missing values in 'itrf_I_6' with NA
#> ! Replaced 25 missing values in 'itrf_I_7' with NA
#> ! Replaced 23 missing values in 'itrf_I_8' with NA
#> ! Replaced 15 missing values in 'itrf_I_9' with NA
#> ! Replaced 15 missing values in 'itrf_I_10' with NA
#> ! Replaced 19 missing values in 'itrf_I_11' with NA
#> ! Replaced 21 missing values in 'itrf_I_12' with NA
#> ! Replaced 21 missing values in 'itrf_I_13' with NA
#> ! Replaced 19 missing values in 'itrf_I_14' with NA
#> ! Replaced 21 missing values in 'itrf_I_15' with NA
#> ! Replaced 21 missing values in 'itrf_I_16' with NA
#> ! Replaced 19 missing values in 'itrf_I_17' with NA
#> ! Replaced 15 missing values in 'itrf_I_19' with NA
#> ! Replaced 22 missing values in 'itrf_I_20' with NA
#> ! Replaced 35 missing values in 'itrf_I_23' with NA
#> ! Replaced 29 missing values in 'itrf_I_24' with NA
#> ! Replaced 19 missing values in 'itrf_E_1' with NA
#> ! Replaced 22 missing values in 'itrf_E_2' with NA
#> ! Replaced 160 missing values in 'itrf_E_3' with NA
#> ! Replaced 73 missing values in 'itrf_E_4' with NA
#> ! Replaced 47 missing values in 'itrf_E_5' with NA
#> ! Replaced 24 missing values in 'itrf_E_6' with NA
#> ! Replaced 20 missing values in 'itrf_E_7' with NA
#> ! Replaced 18 missing values in 'itrf_E_8' with NA
#> ! Replaced 19 missing values in 'itrf_E_9' with NA
#> ! Replaced 19 missing values in 'itrf_E_10' with NA
#> ! Replaced 21 missing values in 'itrf_E_11' with NA
#> ! Replaced 22 missing values in 'itrf_E_12' with NA
#> ! Replaced 23 missing values in 'itrf_E_13' with NA
#> ! Replaced 52 missing values in 'itrf_E_14' with NA
#> ! Replaced 27 missing values in 'itrf_E_15' with NA
#> ! Replaced 15 missing values in 'itrf_E_16' with NA
# check for typos (not allowed values)
dat <- check_values(dat, replace = NA)
#> ! itrf_I_1' invalid at row 3192 (is 7) -> set as NA
#> ! itrf_I_2' invalid at row 4651 (is 9) -> set as NA
#> ! itrf_I_13' invalid at row 3699 (is 4) -> set as NA
#> ! itrf_I_20' invalid at row 2799 (is 4) -> set as NA
#> ! itrf_E_4' invalid at row 2621 (is 6) -> set as NA
#> ! itrf_E_6' invalid at row 2599 (is 9) -> set as NA
#> ! itrf_E_7' invalid at row 2599 (is 9) -> set as NA
#> ! itrf_E_8' invalid at row 2599 (is 9) -> set as NA
#> ! itrf_E_9' invalid at row 2599 (is 9) -> set as NA
#> ! itrf_E_10' invalid at row 2599 (is 9) -> set as NA
#> ! itrf_E_11' invalid at row 2599 (is 9) -> set as NA
#> ! itrf_E_12' invalid at row 2599 (is 9) -> set as NA
#> ! itrf_E_13' invalid at rows 2599, 4146 (is 9, 11) -> set as NA
#> ! itrf_E_14' invalid at row 2599 (is 9) -> set as NA
# Single imputation (EM algorith from the Amelia package)
# based on the variables of the provided scale
dat <- impute_missing(dat, scale == "ITRF" & subscale == "Ext")
#> ╭─ Applied 'Amelia' for single imputation with EM algorithm 
#> ! Imputed 615 values (0.8%)
#> ! Forced 58 imputed values to scales' min and max values.
dat <- impute_missing(dat, scale == "ITRF" & subscale == "Int")
#> ╭─ Applied 'Amelia' for single imputation with EM algorithm 
#> ! Imputed 908 values (1%)
#> ! Forced 71 imputed values to scales' min and max values.
# Show a table with all scales and scale labels included in the data frame
list_scales(dat, levels = c("scale_label", "subscale_label"))
#>                      scale_label subscale_label
#> 1 Integrated teacher report form  Internalizing
#> 2 Integrated teacher report form  Externalizing
# Example with pipeline syntax. Would be much easier to use the "describe" function
# from the psch packages instead of summarise_all here.
dat  |>
  select_items(scale == "ITRF" & subscale == "Ext")  |>
  rename_items(pattern = "{subscale_2}:{name}")  |>
  lapply(function(x) fivenum(x)) |>
  as.data.frame()
#>   OPP.itrf_I_20 APD.itrf_E_1 APD.itrf_E_2 APD.itrf_E_3 APD.itrf_E_4
#> 1             0            0            0            0            0
#> 2             0            0            0            0            0
#> 3             0            1            0            0            0
#> 4             1            2            1            1            1
#> 5             3            3            3            3            3
#>   APD.itrf_E_5 APD.itrf_E_6 OPP.itrf_E_7 OPP.itrf_E_8 OPP.itrf_E_9
#> 1            0            0            0            0            0
#> 2            0            0            0            0            0
#> 3            1            0            0            0            1
#> 4            1            1            0            1            1
#> 5            3            3            3            3            3
#>   OPP.itrf_E_10 OPP.itrf_E_11 OPP.itrf_E_12 OPP.itrf_E_13 APD.itrf_E_14
#> 1             0             0             0             0             0
#> 2             0             0             0             0             0
#> 3             0             0             0             0             0
#> 4             0             1             1             1             1
#> 5             3             3             3             3             3
#>   APD.itrf_E_15 OPP.itrf_E_16
#> 1             0             0
#> 2             0             0
#> 3             0             0
#> 4             1             0
#> 5             3             3
```
