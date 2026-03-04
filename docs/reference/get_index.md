# (Deprecated) Get index

(Deprecated) Get index

## Usage

``` r
get_index(
  data,
  filter = NULL,
  scale = NULL,
  subscale = NULL,
  subscale_2 = NULL,
  names = TRUE,
  class = NULL
)
```

## Arguments

- data:

  A data frame

- filter:

  A logical expression for any dic attribute (e.g. scale == "ITRF" &
  subscale == "Int")

- scale, subscale, subscale_2:

  deprecated

- names:

  If names is TRUE, a vector with variable names will be returned.

- class:

  deprecated

## Value

A vector with indices adhering to the given attribute criteria.
