# Create a dic vector with metadata attributes

Factory function to create a `dic` vector with appropriate attributes.
This function performs parsing and validation of the provided metadata.
It is recommended to use this function to create `dic` objects rather
than manually setting attributes.

## Usage

``` r
dic(
  x,
  item_name = NULL,
  item_label = NULL,
  values = NULL,
  value_labels = NULL,
  missing = NULL,
  weight = 1,
  type = NULL,
  recodes = NULL,
  class = "item",
  ...,
  dic_attributes = NULL,
  .coerce_class = TRUE,
  .format_date = "%Y-%m-%d"
)
```

## Arguments

- x:

  A vector.

- item_name:

  Character.

- item_label:

  Character.

- values:

  Numeric or character vector with values. The vector can be named.

- value_labels:

  Character of the form `value = label; value2 = label2`.

- missing:

  Numeric or character vector with values.

- weight:

  numeric.

- type:

  defaults to data type of x.

- recodes:

  Recoding information e.g. `4 = 1, .default = 0`.

- class:

  default is "item".

- dic_attributes:

  Optional pre-built attribute list (skips parsing if given).

- .coerce_class:

  If TRUE, coerce x to match `type`.

- .format_date:

  Date format for coercion from character -\> Date.

## Details

Details:

- If `item_name` is missing, it is derived from the variable name in the
  calling environment.

- If `type` is missing, it is estimated from the class of `x`.

- If `values` or `value_labels` are provided, they are parsed according
  to the specified `type`.

- If `.coerce_class` is TRUE (default), `x` is coerced to match the
  specified `type`.

- If `type` is "factor", a factor is created from `x` and `values`.

- The resulting object is of class `dic` with appropriate attributes
  set.
