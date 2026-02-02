# Get index of items from dic based on filter expression

Get index of items from dic based on filter expression

## Usage

``` r
get_index_from_dic(data, filter, names = TRUE, class = NULL)
```

## Arguments

- data:

  A data frame

- filter:

  A character string or expression to filter the dic

- names:

  If TRUE, variable names are returned instead of indices

- class:

  If provided, class will be added to the filter expression

## Value

A vector with indices or names of variables adhering to the filter
criteria.
