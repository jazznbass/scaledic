# Impute missing values

Impute missing values in items of a data frame based on their dic
attributes. You can specify a filter to select which items to impute
based on their dic attributes. If force_to_scale is set to TRUE, the
imputed values will be rounded and constrained to the scale's minimum
and maximum values.

## Usage

``` r
impute_missing(
  data,
  filter = NULL,
  method = "continuous",
  force_to_scale = TRUE
)
```

## Arguments

- data:

  A data frame.

- filter:

  A logical expression for any dic attribute (e.g.
  `scale == "ITRF" & subscale == "Int"`).

- method:

  Method for imputation. Either "continuous" (default) or "ordinal". If
  "continuous", the Amelia package will be used for imputation. If
  "ordinal", the mice package will be used for imputation with ordinal
  logistic regression.

- force_to_scale:

  If TRUE, imputed values will be rounded and forced to the scale. That
  is, a value below the scale's minimum or maximum will be set to the
  scale's minimum and maximum. If FALSE, imputed values will not be
  adjusted.

## Value

A data frame with imputed data.

## Details

This function uses the Amelia (continuous) or mice (ordinal) package to
impute missing values in items of a data frame. You can specify a filter
to select which items to impute based on their dic attributes. If
force_to_scale is set to TRUE, the imputed values will be rounded and
constrained to the scale's minimum and maximum values.
