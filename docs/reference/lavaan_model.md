# Extract a model definition that can be applied to lavaan::cfa for a confirmatory factor analysis

Extract a model definition that can be applied to lavaan::cfa for a
confirmatory factor analysis

## Usage

``` r
lavaan_model(scales, adds = "", orthogonal = FALSE)
```

## Arguments

- scales:

  A list with scale information

- adds:

  Additional model definitions

- orthogonal:

  IF TRUE, covariance parameters are set for orthogonal latent factors.

## Value

A character string with a lavaan model definition
