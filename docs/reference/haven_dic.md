# haven labels to dic files

haven labels to dic files

## Usage

``` r
haven_dic(data, remove_haven_class = FALSE)
```

## Arguments

- data:

  A data frame containing variables with haven labels

- remove_haven_class:

  If TRUE, haven labels are removed.

## Value

A data frame with dic information

## Examples

``` r
ex_itrf_dic <- haven_dic(ex_itrf, remove_haven_class = TRUE)
ex_itrf_copy <- dic_haven(ex_itrf_dic)
```
