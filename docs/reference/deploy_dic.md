# Backup and deploy all dictionary information in a data frame

These are workaround functions that store and retrieve all dic
information within a data frame. This is necessary when a function drops
the attributes of a vector.

## Usage

``` r
deploy_dic(data, dic_list)

backup_dic(data)
```

## Arguments

- data:

  A data frame.

- dic_list:

  A list created by the backup_dic() function.

## Value

A data frame with added dic information from dic_list.

A list containing all dic information from the data frame.

## Examples

``` r
## create a copy of ex_itrf and strip all dic information
copy_ex_itrf <- ex_itrf
copy_ex_itrf[] <- lapply(copy_ex_itrf, function(x) {attr(x, "dic") <- NULL; x})
attr(copy_ex_itrf, "dic") <- NULL

## backup dic information from ex_itrf and deploy to copy_ex_itrf
dic_list <- backup_dic(ex_itrf)
copy_ex_itrf <- deploy_dic(copy_ex_itrf, dic_list)

## test that both are identical
identical(ex_itrf, copy_ex_itrf)
#> [1] TRUE
```
