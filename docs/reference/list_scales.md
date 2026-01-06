# List scales

List scales

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

  The target data frame

- levels:

  Character vector with names of dic attributes used to extract scale
  information.

- n_items:

  If TRUE, number of items for each scale is shown

- char_na:

  Character for NAs.

## Value

A data.frame with scales on different levels
