# Build a dictionary file template for scale definition

This function builds an empty skeleton of a dictionary file for scale
definition. It can either write an Excel file or return a data frame.

## Usage

``` r
build_scaledic_skeleton(filename = NA, nrows = 0)
```

## Arguments

- filename:

  Character string. Default is 'dic_template.xlsx'. If NA, returns a
  data frame instead of writing an Excel file.

- nrows:

  Number of empty rows added to data frame. Default is 0.

## Value

When 'filename' is not empty, it writes an Excel file with an empty
template of a dic file. When 'filename' is NA, returns a data frame.

## Details

The skeleton contains all necessary variable names for a dic file. The
user can specify the number of empty rows to be added to the data frame.
This is useful when creating a dic file for a data frame with many
variables. The function uses the variable names defined in the option
'dic_file_vars'. These can be modified by the user if needed. The
function uses the 'openxlsx' package to write the Excel file. Make sure
to have it installed.

## Examples

``` r
build_scaledic_skeleton(NA, nrows = 3)
#>   item_name item_label weight values value_labels missing type class
#> 1        NA         NA     NA     NA           NA      NA   NA    NA
#> 2        NA         NA     NA     NA           NA      NA   NA    NA
#> 3        NA         NA     NA     NA           NA      NA   NA    NA
#>   score_filter score_function
#> 1           NA             NA
#> 2           NA             NA
#> 3           NA             NA
```
