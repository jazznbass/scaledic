# Table with alpha values and item statistics for multiple scales

Returns a data.frame with item analyses for the provided scales. Useful
for reporting scale reliabilities and item statistics in manuscripts.

## Usage

``` r
alpha_table(
  data,
  scales,
  labels = NULL,
  round = 2,
  CI = TRUE,
  conf_level = 0.95,
  check_key = TRUE,
  keys = NULL,
  keys_from_weights = TRUE,
  RMSEA = FALSE,
  difficulty = FALSE,
  values = NULL,
  fa = TRUE
)
```

## Arguments

- data:

  A data Frame or tibble containing the item responses.

- scales:

  A list containing vectors with variable names. Each list element
  defines one scale. Named list elements are used as labels.

- labels:

  Label names for scales (defaults to named list elements in 'scales').

- round:

  Rounds values to given decimal position.

- CI:

  If TRUE confidence intervals are calculated.

- conf_level:

  Confidence level (e.g. 0.95 for 95 percent).

- check_key:

  Check_key for the psych::alpha function.

- keys:

  Optional key argument for the psych::alpha function.

- keys_from_weights:

  If TRUE, tries to define keys from scaledics "weights" parameter.

- RMSEA:

  If TRUE RMSEA is calculated.

- difficulty:

  If TRUE, the difficulty of the item is calculated.

- values:

  Sets maximum and minimum valid values necessary to calculate item
  difficulty

- fa:

  If TRUE, a one factor exploratory factor analyses is calculated and
  loadings are reported.

## Value

A data frame with concise scale indices.

## Details

The function uses the psych::alpha function to calculate Cronbach's
alpha and standardized alpha. Additionally, it calculates item means,
standard deviations, item discriminations (item-total correlations), and
item difficulties (if requested). If the 'fa' argument is set to TRUE, a
one-factor exploratory factor analysis is performed, and the minimum and
maximum absolute factor loadings are reported. The function can also
compute confidence intervals for alpha and standardized alpha. It
handles missing data by removing rows with all items missing for each
scale. Variables with zero variance are automatically excluded from the
analysis, and a message is displayed indicating which variables were
dropped. The resulting data frame contains concise scale indices, making
it easy to report scale reliabilities and item statistics in
manuscripts.

## See also

[`alpha`](https://rdrr.io/pkg/psych/man/alpha.html),
[`fa`](https://rdrr.io/pkg/psych/man/fa.html), [`dic_attr`](dic_attr.md)

## Author

Jürgen Wilbert

## Examples

``` r
scales <- get_scales(ex_itrf,
  Int = scale == "ITRF" & subscale == "Int",
  Ext = scale == "ITRF" & subscale == "Ext"
)
alpha_table(ex_itrf, scales = scales, difficulty = TRUE, values = list(c(0, 3)), RMSEA = TRUE)
#> Removed 4 rows because all items were missing.
#> Error in map_dbl(data_scale, ~as.numeric(scaledic::dic_attr(.x, "weight"))): could not find function "map_dbl"
```
