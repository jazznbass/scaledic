# Extracts a list of item names based on dic information

This function extracts item names from a data frame based on dic
information and returns them as a list. Each entry in the list
corresponds to a scale defined by a logical expression or by all
instances of a dic attribute.

## Usage

``` r
get_scales(data, ...)
```

## Arguments

- data:

  A data.frame with dic information.

- ...:

  A logical expression defining a selection definition or a name
  defining a dic attribute for automatic scale definition.

## Value

A (named) list with character vectors of item names.

## Details

This function is basically a wrapper around the
`select_items(data = data, filter = ..., names_only = TRUE)` function.
It takes multiple filter expressions for a single data frame and returns
the names. This is mostly helpful for functions that take multiple scale
definitions like [`alpha_table()`](alpha_table.md). If you provide a
name instead of a logical expression, that name must be a dic attribute.
A list of scales will be created based on all values of that attribute.

## Examples

``` r
## define individual scales
get_scales(ex_itrf,
  'APD' = subscale_2 == "APD",
  'OPP' = subscale_2 == "OPP",
  "SW" = subscale_2 == "SW",
  'AD' = subscale_2 == "AD"
)
#> $APD
#> [1] "itrf_E_1"  "itrf_E_2"  "itrf_E_3"  "itrf_E_4"  "itrf_E_5"  "itrf_E_6" 
#> [7] "itrf_E_14" "itrf_E_15"
#> attr(,"filter")
#> [1] "subscale_2 == \"APD\""
#> 
#> $OPP
#> [1] "itrf_I_20" "itrf_E_7"  "itrf_E_8"  "itrf_E_9"  "itrf_E_10" "itrf_E_11"
#> [7] "itrf_E_12" "itrf_E_13" "itrf_E_16"
#> attr(,"filter")
#> [1] "subscale_2 == \"OPP\""
#> 
#> $SW
#> [1] "itrf_I_1"  "itrf_I_4"  "itrf_I_5"  "itrf_I_6"  "itrf_I_13" "itrf_I_14"
#> [7] "itrf_I_16" "itrf_I_24"
#> attr(,"filter")
#> [1] "subscale_2 == \"SW\""
#> 
#> $AD
#>  [1] "itrf_I_2"  "itrf_I_7"  "itrf_I_8"  "itrf_I_9"  "itrf_I_10" "itrf_I_11"
#>  [7] "itrf_I_12" "itrf_I_15" "itrf_I_17" "itrf_I_19" "itrf_I_23"
#> attr(,"filter")
#> [1] "subscale_2 == \"AD\""
#> 

## generate scale list based on all values of a dic attribute
get_scales(ex_itrf, subscale_2)
#> $SW
#> [1] "itrf_I_1"  "itrf_I_4"  "itrf_I_5"  "itrf_I_6"  "itrf_I_13" "itrf_I_14"
#> [7] "itrf_I_16" "itrf_I_24"
#> attr(,"filter")
#> subscale_2 == "SW"
#> 
#> $AD
#>  [1] "itrf_I_2"  "itrf_I_7"  "itrf_I_8"  "itrf_I_9"  "itrf_I_10" "itrf_I_11"
#>  [7] "itrf_I_12" "itrf_I_15" "itrf_I_17" "itrf_I_19" "itrf_I_23"
#> attr(,"filter")
#> subscale_2 == "AD"
#> 
#> $OPP
#> [1] "itrf_I_20" "itrf_E_7"  "itrf_E_8"  "itrf_E_9"  "itrf_E_10" "itrf_E_11"
#> [7] "itrf_E_12" "itrf_E_13" "itrf_E_16"
#> attr(,"filter")
#> subscale_2 == "OPP"
#> 
#> $APD
#> [1] "itrf_E_1"  "itrf_E_2"  "itrf_E_3"  "itrf_E_4"  "itrf_E_5"  "itrf_E_6" 
#> [7] "itrf_E_14" "itrf_E_15"
#> attr(,"filter")
#> subscale_2 == "APD"
#> 
```
