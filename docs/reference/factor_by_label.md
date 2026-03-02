# Turns a dic variable into a factor based on the value labels

This function takes a vector with dic information and converts it into a
factor based on the value labels stored in the dic attributes.

## Usage

``` r
factor_by_label(x, ordered)
```

## Arguments

- x:

  A vector with dic information.

- ordered:

  If TRUE, the resulting factor will be ordered. Default is FALSE.

## Value

A factor with levels based on the value labels. If no value labels are
present, an empty factor is returned.

## Examples

``` r
# Apply a dictionary to data
dat_dic <- apply_dic(ex_scaledic_data, ex_scaledic_dic)
#> ! Replaced 1 missing value in 'rel_3' with NA
#> ! Replaced 1 missing value in 'rel_4' with NA
#> ! Replaced 1 missing value in 'sui_2' with NA
#> ! Replaced 1 missing value in 'age' with NA
# Proportions of responses
dat_dic$rel_2 |> factor_by_label() |> table() |> prop.table()
#> Error in factor_by_label(dat_dic$rel_2): argument "ordered" is missing, with no default
# A cross table
table(factor_by_label(dat_dic$rel_1), dat_dic$gender)
#> Error in factor_by_label(dat_dic$rel_1): argument "ordered" is missing, with no default
```
