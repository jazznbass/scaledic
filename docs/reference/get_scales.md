# Extracts a list of item names based on dic information

Extracts a list of item names based on dic information

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
the item names. This is mostly helpful for functions that take multiple
scale definitions like [`alpha_table()`](alpha_table.md). If you provide
a name instead of a logical expression, that name must be a dic
attribute. A list of scales will be created based on all values of that
attribute.

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
#> 
#> $OPP
#> [1] "itrf_I_20" "itrf_E_7"  "itrf_E_8"  "itrf_E_9"  "itrf_E_10" "itrf_E_11"
#> [7] "itrf_E_12" "itrf_E_13" "itrf_E_16"
#> 
#> $SW
#> [1] "itrf_I_1"  "itrf_I_4"  "itrf_I_5"  "itrf_I_6"  "itrf_I_13" "itrf_I_14"
#> [7] "itrf_I_16" "itrf_I_24"
#> 
#> $AD
#>  [1] "itrf_I_2"  "itrf_I_7"  "itrf_I_8"  "itrf_I_9"  "itrf_I_10" "itrf_I_11"
#>  [7] "itrf_I_12" "itrf_I_15" "itrf_I_17" "itrf_I_19" "itrf_I_23"
#> 

## generate scale list based on all instances of an attribute
get_scales(ex_itrf, subscale_2)
#> $SW
#> [1] "itrf_I_1"  "itrf_I_4"  "itrf_I_5"  "itrf_I_6"  "itrf_I_13" "itrf_I_14"
#> [7] "itrf_I_16" "itrf_I_24"
#> 
#> $AD
#>  [1] "itrf_I_2"  "itrf_I_7"  "itrf_I_8"  "itrf_I_9"  "itrf_I_10" "itrf_I_11"
#>  [7] "itrf_I_12" "itrf_I_15" "itrf_I_17" "itrf_I_19" "itrf_I_23"
#> 
#> $OPP
#> [1] "itrf_I_20" "itrf_E_7"  "itrf_E_8"  "itrf_E_9"  "itrf_E_10" "itrf_E_11"
#> [7] "itrf_E_12" "itrf_E_13" "itrf_E_16"
#> 
#> $APD
#> [1] "itrf_E_1"  "itrf_E_2"  "itrf_E_3"  "itrf_E_4"  "itrf_E_5"  "itrf_E_6" 
#> [7] "itrf_E_14" "itrf_E_15"
#> 
```
