# Build and validate dic attribute list

Build and validate dic attribute list

## Usage

``` r
create_dic_attributes(
  x = NULL,
  item_name = NULL,
  item_label = NULL,
  values = NULL,
  value_labels = NULL,
  missing = NULL,
  weight = 1,
  type = NULL,
  recodes = NULL,
  class = "item",
  ...
)
```

## Arguments

- x:

  Optional. Used for type estimation only if `type` missing.

- validate:

  If FALSE: minimal normalization only (no parsing/checks).
