# List scales in a data frame based on dic attributes

Lists scales and subscales defined in the dic attributes of a data
frame. This is useful for getting an overview of the scales present in a
dataset that uses dic attributes to define scales.

## Usage

``` r
list_scales(
  data,
  levels = c("scale", "subscale", "subscale_2"),
  n_items = FALSE,
  char_na = ""
)
```

## Arguments

- data:

  The target data frame.

- levels:

  Character vector with names of dic attributes used to extract scale
  information.

- n_items:

  If TRUE, number of items for each scale is shown.

- char_na:

  Character for NAs.

## Value

A data.frame with scales on different levels.

## Details

The function extracts scale information from dic attributes of variables
in the data frame. By default, it looks for attributes named "scale",
"subscale", and "subscale_2". It can also count the number of items for
each scale or subscale if requested.
