# Select items

Selects a data frame with variables of a specific scale

## Usage

``` r
select_items(data, filter = NULL, names_only = FALSE, index_only = FALSE)
```

## Arguments

- data:

  A data frame with dic information

- filter:

  A logical expression for any dic attribute (e.g.
  `scale == "ITRF" & subscale == "Int"`)

- names_only:

  If TRUE, variable names are returned instead of a data frame

- index_only:

  If TRUE, variable indices are returned instead of a data frame

## Value

A data frame, a vector with variable names, or a vector with indices.
