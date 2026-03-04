# Extract a model definition that can be applied to lavaan::cfa for a confirmatory factor analysis based on provided scales and items.

Extract a model definition that can be applied to lavaan::cfa for a
confirmatory factor analysis based on provided scales and items.

## Usage

``` r
lavaan_model(scales, adds = "", orthogonal = FALSE)
```

## Arguments

- scales:

  A list with scale information. Each element in the list corresponds to
  a scale and contains a character vector with item names.

- adds:

  Additional model definitions as a character string.

- orthogonal:

  IF TRUE, covariance parameters are set for orthogonal latent factors.

## Value

A character string with a lavaan model definition.

## Details

- Each scale in `scales` is defined as a latent factor, with items as
  observed indicators.

- If `orthogonal = TRUE`, covariance parameters between latent factors
  are set to zero.

- Additional model definitions can be added via `adds`.
