# Low-level constructor for dic vectors

Creates a dic vector with specified attributes. Detailed parsing and
validation is not performed by this function. Use the [`dic()`](dic.md)
factory function for that purpose.

## Usage

``` r
new_dic(x, dic_attributes = list(), label = NULL, labels = NULL)
```

## Arguments

- x:

  An atomic vector (integer/double/logical/character/factor/Date, ...)

- dic_attributes:

  A named list with dic metadata.

- label:

  Optional haven-style variable label.

- labels:

  Optional haven-style value labels (named vector).

## Value

A dic vector with specified attributes.

## Details

Details:

- The function sets the class of `x` to include "dic".

- The provided `dic_attributes` list is assigned as the "dic" attribute.

- Optional haven-style variable label and value labels can be set.
