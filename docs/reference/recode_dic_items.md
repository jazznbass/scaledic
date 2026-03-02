# Recode Dictionary Items

This function takes a data frame and recodes variables based on the
`recodes` attribute stored in the dic attributes of a variable.

## Usage

``` r
recode_dic_items(df, prefix_label = "(recoded)")
```

## Arguments

- df:

  a data frame.

- prefix_label:

  Prefix is added to the item label of a recoded item.

## Value

A data frame with recoded variables.

## Details

For each variable in the data frame, if a `recodes` attribute is found,
the function creates a new variable where the values are recoded
according to the specified recoding rules. The original variable is
replaced with the recoded variable, and the item label is updated to
indicate that the variable has been recoded.

## Author

Juergen Wilbert

## Examples

``` r
q1 <- dic(
  x = c(1,1,2,3,1,3,4,4,3,2,4,5),
  item_name = "knowledge_1",
  item_label = "What is the capital of Germany?",
  type = "integer",
  weight = 1,
  values = "1:4",
  value_labels = "1 = Brussels; 2 = Hamburg; 3 = Bonn; 4 = Berlin",
  recodes = "1 = -1; 2 = 0; 3 = 0; 4 = 1; .default = NA"
)
q1
#> ║What is the capital of Germany? 
#> ║Data type is integer
#> ║Valid values: 1:4
#> ║Value labels:
#> ║  1 = Brussels
#> ║  2 = Hamburg
#> ║  3 = Bonn
#> ║  4 = Berlin
#> ║Recodes:
#> ║  1 = -1
#> ║  2 = 0
#> ║  3 = 0
#> ║  4 = 1
#> ║  .default = NA
#> ║ 
#> ║Length is 12 (0 NA; 1 invalid)
#> ║ [1] 1 1 2 3 1 3 4 4 3 2 4 5
recode_dic_items(q1)
#> ! Found recoding information and recoded values.
#> ║(recoded) What is the capital of Germany? 
#> ║Data type is integer
#> ║Valid values: -1:1
#> ║ 
#> ║Length is 12 (1 NA; 0 invalid)
#> ║ [1] -1 -1  0  0 -1  0  1  1  0  0  1 NA
```
