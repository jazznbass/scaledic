# Automatic scale scoring from a dic file

This function is deprecated. Please score scales directly from within R.
Use the [`get_scales()`](get_scales.md) and
[`score_scale()`](score_scale.md) functions. This function takes a data
frame and a dic file with scale definitions and applies the specified
scoring functions to create new variables for each scale. The scoring
functions can be specified in the dic file, and if not provided, it
defaults to using the mean of the items in the scale. The new variables
will be named according to the 'item_name' column in the dic file. The
function first checks for the presence of dic information either in the
data frame's attributes or as a separate argument. It then iterates
through each scale defined in the dic file, applies the specified
scoring function to the relevant items, and creates new variables for
each scale. Finally, it returns a data frame with the new scale scores.

## Usage

``` r
score_from_dic(data, dic = NULL)
```

## Arguments

- data:

  A data frame with dic information. The dic information can be provided
  as an attribute of the data frame or as a separate argument.

- dic:

  A dic file with scoring information. If NULL, the function will
  attempt to retrieve dic information from the data frame's attributes.

## Value

A data frame with added scale scores. The new variables will be named
according to the 'item_name' column in the dic file.
