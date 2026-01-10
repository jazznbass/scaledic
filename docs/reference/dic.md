# Create a dic vector (factory)

Create a dic vector (factory)

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

  Character of the form `value = label; value2 = label2`

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
