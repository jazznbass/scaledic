# Impute missing values

Impute missing values in items using the Amelia package.

## Usage

``` r
impute_missing(data, filter = NULL, force_to_scale = TRUE)
```

## Arguments

- data:

  A data frame.

- filter:

  A logical expression for any dic attribute (e.g.
  `scale == "ITRF" & subscale == "Int"`).

- force_to_scale:

  If TRUE, imputed values will be rounded and forced to the scale. That
  is, a value below the scale's minimum or maximum will be set to the
  scale's minimum and maximum. If FALSE, imputed values will not be
  adjusted.

## Value

A data frame with imputed data.

## Details

This function uses the Amelia package to impute missing values in items
of a data frame. You can specify a filter to select which items to
impute based on their dic attributes. If force_to_scale is set to TRUE,
the imputed values will be rounded and constrained to the scale's
minimum and maximum values.
