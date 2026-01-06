# Impute missing values

Impute missing values

## Usage

``` r
impute_missing(data, filter = NULL, force_to_scale = TRUE)
```

## Arguments

- data:

  A data frame

- filter:

  A logical expression for any dic attribute (e.g. scale == "ITRF" &
  subscale == "Int")

- force_to_scale:

  If TRUE, imputed values will be rounded and forced to the scale. That
  is, a value below the scale's minimum or maximum will be set to the
  scale's minimum and maximum.

## Value

A data frame with imputed data.
