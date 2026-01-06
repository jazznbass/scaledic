# Turns a dic variable into a factor based on the value labels

Turns a dic variable into a factor based on the value labels

## Usage

``` r
factor_by_label(x)
```

## Arguments

- x:

  A vector with dic information.

## Value

A factor with levels based on the value labels.

## Examples

``` r
# Apply a dictionary to data
dat_dic <- apply_dic(ex_scaledic_data, ex_scaledic_dic)
#> ! (replace_missing)
#> 1: Replaced 1 missing value in 'age' with NA
#> 2: Replaced 1 missing value in 'rel_3' with NA
#> 3: Replaced 1 missing value in 'rel_4' with NA
#> 4: Replaced 1 missing value in 'sui_2' with NA
# Proportions of responses
dat_dic$rel_2 |> factor_by_label() |> table() |> prop.table()
#> 
#>        Rarely or never    A few times a month            Once a week 
#>              0.2631579              0.1578947              0.1052632 
#> Two or more times/week                  Daily   More than once a day 
#>              0.1052632              0.2105263              0.1578947 
# A cross table
table(factor_by_label(dat_dic$rel_1), dat_dic$gender)
#>                      
#>                       m f d
#>   Never               0 1 1
#>   Once a year or less 1 2 1
#>   A few times a year  1 1 3
#>   A few times a month 0 1 0
#>   Once a week         1 1 0
#>   More than once/week 4 1 0
```
