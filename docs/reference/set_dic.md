# Set dictionary information to variables

Set dictionary information to variables

## Usage

``` r
set_dic(data, .vars = NULL, ...)
```

## Arguments

- data:

  A data frame or a vector.

- .vars:

  Character vector with variable names. If data is a data frame, address
  these variables. If left `NULL` and data is a data frame, all
  variables from the data frame are addressed.

- ...:

  dic attributes of the form `attribute = value`.

## Value

A data frame or a vector with added dic attributes.

## Details

Standard attributes are: `"item_name"`, `"item_label"`, `"weight"`,
`"type"`, `"values"`, `"value_labels"`, `"missing"`.

## Examples

``` r
hap_1 <- sample(1:5, 30, replace = TRUE)
set_dic(hap_1,
   item_label = "How do you feel today?",
   scale = "hap",
   scale_label = "Happiness",
   values = 1:3,
   value_labels = "1 = not happy; 2 = in between; 3 = happy"
)
#> ! (.set_dic)
#> 1: Attribute 'item_name' missing and set to 'hap_1'.
#> 2: Attribute 'type' missing and set to 'integer'.
#> 3: Attribute 'weight' missing and set to 1.
#> ║How do you feel today? (Happiness) 
#> ║Data type is integer
#> ║Valid values: 1:3
#> ║Value labels:
#> ║  1 = not happy
#> ║  2 = in between
#> ║  3 = happy
#> ║Length: 30 (0 NA; 10 invalid)
#> ║ [1] 3 4 2 1 1 4 2 1 4 5 5 5 2 1 5 1 1 3 2 1 4 1 3 2 1 5 3 4 3 1
```
