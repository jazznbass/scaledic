# Identify invalid values in a vector according to dic attributes

Identify invalid values in a vector according to dic attributes

## Usage

``` r
id_invalid_values(x, include_missing = FALSE, integer_as_numeric = TRUE)
```

## Arguments

- x:

  A dic vector

- include_missing:

  If TRUE, missing values (provided as 'missing' in the dic file) will
  be considered as valid values. If FALSE, missing values will be
  considered as invalid values.

- integer_as_numeric:

  If TRUE, type 'integer' will be handled as 'numeric'.

## Value

A vector with indices of invalid values
