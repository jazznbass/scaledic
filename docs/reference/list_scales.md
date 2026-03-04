# List scales in a data frame based on dic attributes

Lists scales and subscales defined in the dic attributes of a data
frame. This is useful for getting an overview of the scales present in a
dataset that uses dic attributes to define scales.

## Usage

``` r
list_scales(data, ..., levels = NULL, .n_items = FALSE, .char_na = "")
```

## Arguments

- data:

  The target data frame.

- ...:

  Additional dic attribute names to include in the output. Overrides the
  `levels` argument if provided.

- levels:

  Character vector with names of dic attributes used to extract scale
  information.

- .n_items:

  If TRUE, number of items for each scale is added in parentheses.

- .char_na:

  Character for NAs.

## Value

A data.frame with scales on different levels.

## Details

The function extracts scale information from dic attributes of variables
in the data frame. By default, it looks for attributes named "scale",
"subscale", and "subscale_2". It can also count the number of items for
each scale or subscale if requested.

## Examples

``` r
## List default scales
ex_itrf |> list_scales()
#>   scale subscale subscale_2
#> 1  ITRF      Int         SW
#> 2  ITRF      Int         AD
#> 3  ITRF      Ext        OPP
#> 4  ITRF      Ext        APD

## List custom scale levels
ex_itrf |> list_scales("subscale_label", "subscale_2_label")
#>   subscale_label                      subscale_2_label
#> 1  Externalizing               Oppositional/Disruptive
#> 2  Externalizing Academic Productivity/Disorganization
#> 3  Internalizing                    Socially Withdrawn
#> 4  Internalizing                     Anxious/Depressed
```
