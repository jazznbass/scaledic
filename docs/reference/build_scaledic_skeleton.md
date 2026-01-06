# Build a dictionary file template

Build a dictionary file template

## Usage

``` r
build_scaledic_skeleton(filename = "dic_template.xlsx", nrows = 0)
```

## Arguments

- filename:

  Character string. Default is 'dic_template.xlsx'. If NA, returns a
  data frame instead of writing an Excel file.

- nrows:

  Number of empty rows added to data frame.

## Value

When 'filename' is not empty, it writes an Excel file with an empty
template of a dic file. When 'filename' is NA, returns a data frame.

## Examples

``` r
build_scaledic_skeleton(NA, nrows = 3)
#>   item_label weight values value_labels missing type class score_filter
#> 1         NA     NA     NA           NA      NA   NA    NA           NA
#> 2         NA     NA     NA           NA      NA   NA    NA           NA
#> 3         NA     NA     NA           NA      NA   NA    NA           NA
#>   score_function
#> 1             NA
#> 2             NA
#> 3             NA
```
