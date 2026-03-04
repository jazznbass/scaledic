# (Deprecated) Rename long to short

Set names of items as long labels and vice versa.

## Usage

``` r
names2item(
  data,
  chars = NULL,
  prefix = "",
  char_sep = "_",
  char_weight = c("(-)", "(+)"),
  char_prefix_end = ": "
)

names2label(data)
```

## Arguments

- data:

  A data frame

- chars:

  If not NULL, only the first n chars og the long label will be applied.

- prefix:

  A character string or vector of character strings defining a prefix.
  May include "scale", "subscale", "subscale2", "index", "reverse",
  "weight", or "najme".

- char_sep:

  Character with separator between prefix information.

- char_weight:

  Character vector of length two with signs for negative and positive
  weights.

- char_prefix_end:

  Character with separator between prefix and item.

## Value

A renamed data frame

## Details

names2item renames to the long label. names2label renames to the short
label.
