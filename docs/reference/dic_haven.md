# Create haven labels and value labels from dic files

Convert dic files to haven labelled data frames

## Usage

``` r
dic_haven(data, overwrite = TRUE)
```

## Arguments

- data:

  A data frame containing `dic` information.

- overwrite:

  Logical. If `TRUE`, overwrites existing haven labels.

## Value

A data frame with haven labels and value labels.

## Details

This function converts a data frame containing dic information into a
data frame with haven labels. Item labels and value labels are extracted
from the dic attributes and stored as haven attributes.

## Examples

``` r
ex_itrf_copy <- haven_dic(ex_itrf, remove_haven_class = TRUE)
ex_itrf_copy <- dic_haven(ex_itrf)
identical(ex_itrf, ex_itrf_copy)
#> [1] TRUE
```
