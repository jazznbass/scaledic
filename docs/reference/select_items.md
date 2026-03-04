# Select items from a data frame based on dictionary attributes

Selects a data frame with variables of a specific scale based on dic
attributes.

## Usage

``` r
select_items(data, filter = NULL, names_only = FALSE, index_only = FALSE)
```

## Arguments

- data:

  A data frame with dic information.

- filter:

  A logical expression for any dic attribute (e.g.
  `scale == "ITRF" & subscale == "Int"`) to filter the items.

- names_only:

  If TRUE, variable names are returned instead of a data frame.

- index_only:

  If TRUE, variable indices are returned instead of a data frame.

## Value

A data frame, a vector with variable names, or a vector with indices.

## Details

This function allows you to filter and select variables from a data
frame based on their dictionary attributes. You can specify a logical
expression using any dic attribute (e.g., `scale`, `subscale`,
`subscale_2`, etc.) to filter the items you want to select. For example,
to select all items that belong to the "ITRF" scale and the "Int"
subscale, you would use the filter expression
`scale == "ITRF" & subscale == "Int"`. This function is particularly
useful for working with large datasets where you need to extract
specific subsets of items based on their characteristics defined in the
dictionary. It returns either a data frame with the selected items, a
vector of variable names, or a vector of indices, depending on the
parameters you provide.

## Examples

``` r
## select items belonging to the "ITRF" scale and "Int" subscale
selected_items <- select_items(
  data = ex_itrf,
  filter = scale == "ITRF" & subscale == "Int"
 )
 head(selected_items)
#>   itrf_I_1 itrf_I_2 itrf_I_4 itrf_I_5 itrf_I_6 itrf_I_7 itrf_I_8 itrf_I_9
#> 1        1        0        0        0        0        0        0        0
#> 2        0        0        0        0        0        0        0        0
#> 3        0        2        0        0        0        0        2        0
#> 4        1        1        3        0        3        0        2        1
#> 5        0        1        0        0        0        0        0        0
#> 6        0        0        0        0        0        0        0        0
#>   itrf_I_10 itrf_I_11 itrf_I_12 itrf_I_13 itrf_I_14 itrf_I_15 itrf_I_16
#> 1         0         0         0         0         0         0         0
#> 2         0         0         0         0         0         0         0
#> 3         0         0         0         0         0         0         0
#> 4         2         2         1         0         0         2         2
#> 5         0         0         0         1         0         1         0
#> 6         0         0         2         0         0         0         0
#>   itrf_I_17 itrf_I_19 itrf_I_23 itrf_I_24
#> 1         0         0         0         0
#> 2         1         0         0         0
#> 3         0         0         0         0
#> 4         1         0         1         2
#> 5         0         1         0         0
#> 6         0         0         0         0
 ## get names of selected items
 item_names <- select_items(
  data = ex_itrf,
  filter = scale == "ITRF" & subscale == "Int",
  names_only = TRUE
 )
 print(item_names)
#>  [1] "itrf_I_1"  "itrf_I_2"  "itrf_I_4"  "itrf_I_5"  "itrf_I_6"  "itrf_I_7" 
#>  [7] "itrf_I_8"  "itrf_I_9"  "itrf_I_10" "itrf_I_11" "itrf_I_12" "itrf_I_13"
#> [13] "itrf_I_14" "itrf_I_15" "itrf_I_16" "itrf_I_17" "itrf_I_19" "itrf_I_23"
#> [19] "itrf_I_24"
 ## get indices of selected items
 item_indices <- select_items(
  data = ex_itrf,
  filter = scale == "ITRF" & subscale == "Int",
  index_only = TRUE
 )
 print(item_indices)
#>  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 19 20
```
