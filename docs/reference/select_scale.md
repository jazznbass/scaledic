# (Deprecated) Select a scale

Selects a data frame with variables of a specific scale

## Usage

``` r
select_scale(
  data,
  filter = NULL,
  scale = NULL,
  subscale = NULL,
  subscale_2 = NULL
)
```

## Arguments

- data:

  A data frame with dic information

- filter:

  A logical expression for any dic attribute (e.g. scale == "ITRF" &
  subscale == "Int")

- scale, subscale, subscale_2:

  deprecated

## Value

A data frame
