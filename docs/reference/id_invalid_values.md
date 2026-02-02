# Identify invalid values in a vector according to dic attributes

Identify invalid values in a vector according to dic attributes

## Usage

``` r
id_invalid_values(x, include_missing = FALSE)
```

## Arguments

- x:

  A dic vector

- include_missing:

  If TRUE, missing values (provided as 'missing' in the dic file) will
  be considered as valid values. If FALSE, missing values will be
  considered as invalid values.

## Value

A vector with indices of invalid values
