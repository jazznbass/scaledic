# Recode Dictionary Items

This function takes a data frame and recodes specified variables based
on the dictionary.

## Usage

``` r
recode_dic_items(df, prefix_label = "(recoded)")
```

## Arguments

- df:

  a data frame that contains variables to be recoded

- prefix_label:

  Prefix is added to the item label of a recoded item

## Value

A recoded data frame

## Details

This function is useful, when you want to recode the 'raw' values from a
vector in a data.frame based on recoding information provided in a
dictionary file. For example, you code the answers in a data frame that
were given to a task. And you have additional information in a dic-file
that tells you, which answer is correct (1) vs. false (0).

## Examples

``` r
q1 <- new_dic(
  x = c(1,1,2,3,1,3,4,4,3,2,4,5),
  item_name = "knowledge_1",
  item_label = "What is the capital of Germany?",
  type = "integer",
  weight = 1,
  values = "1:4",
  value_labels = "1 = Brussels; 2 = Hamburg; 3 = Bonn; 4 = Berlin",
  recodes = "1 = -1; 2 = 0; 3 = 0; 4 = 1"
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
#> ║Length: 12 (0 NA; 1 invalid)
#> ║ [1] 1 1 2 3 1 3 4 4 3 2 4 5
recode_dic_items(q1)
#> ! (.recode_dic_items)
#> 1: Found recoding information and recoded values.
#> ║(recoded) What is the capital of Germany? 
#> ║Data type is integer
#> ║Valid values: -1:1
#> 
#> ║Length: 12 (1 NA; 0 invalid)
#> ║ [1] -1 -1  0  0 -1  0  1  1  0  0  1 NA
```
