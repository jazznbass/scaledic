# Score scale calculates scale scores.

Score scale calculates scale scores.

## Usage

``` r
score_scale(
  data,
  filter,
  sum = FALSE,
  min_valid = 1,
  max_na = NA,
  label = NULL,
  fun = NULL,
  var_weight = NULL,
  var_recoding = "recodes"
)
```

## Arguments

- data:

  A data frame

- filter:

  A logical expression for any *dic* attribute (e.g.
  `scale == "ITRF" & subscale == "Int"`)

- sum:

  If `FALSE`, a weighted mean function is applied for building the
  scores. If `TRUE`, a weighted sum function is applied. When argument
  fun is set, `sum` is ignored.

- min_valid:

  Minimal number of valid values that is required for calculating the
  mean. A value between 0 and 1 indicates a proportion of values (e.g.,
  0.5 = 50 percent of values have to be valid).

- max_na:

  Maximum number of NAs that are allowed before returning NA. A value
  between 0 and 1 indicates a proportion of values (e.g., 0.5 = 50
  percent NAs are allowed).

- label:

  A character string with a label for the resulting score variable.
  Automatically generated if label is not set.

- fun:

  A function for calculating the score (e.g., `weighted.median`). See
  details.

- var_weight:

  Name of the *dic* attribute that is applied to derive weights.
  Defaults to `weight`.

- var_recoding:

  Name if the *dic* attribute that may contain recoding information.
  Defaults to `scores`.

## Value

A data frame

## Details

If you provide your own function, the first argument of that function
must take the vector of values and the second argument the weights.

## Examples

``` r
dat <- apply_dic(ex_scaledic_data, ex_scaledic_dic)
#> ! (replace_missing)
#> 1: Replaced 1 missing value in 'age' with NA
#> 2: Replaced 1 missing value in 'rel_3' with NA
#> 3: Replaced 1 missing value in 'rel_4' with NA
#> 4: Replaced 1 missing value in 'sui_2' with NA
# apply the default weighted mean function
score_scale(dat, scale == "rel", label = "Religious beliefs")
#> Religious beliefs
#> (weighted mean of items: scale == "rel") 
#> ║Data type is numeric
#> ║Valid values: From 1 to 5.4
#> ║Value labels:
#> ║  1 = min
#> ║  5.4 = max
#> ║Length: 20 (0 NA; 2 invalid)
#> ║ [1]  2.20  1.60 15.20  2.60  3.80  3.60  2.40  3.40  4.40  4.80 15.40  3.40
#> ║[13]  3.80  2.25  2.60  3.80  3.80  4.50  4.20  3.00
# apply the weighted sum function
score_scale(dat, scale == "rel", label = "Religious beliefs", sum = TRUE)
#> Religious beliefs
#> (weighted sum of items: scale == "rel") 
#> ║Data type is numeric
#> ║Valid values: From 5 to 27
#> ║Value labels:
#> ║  5 = min
#> ║  27 = max
#> ║Length: 20 (0 NA; 2 invalid)
#> ║ [1] 11  8 76 13 19 18 12 17 22 24 77 17 19  9 13 19 19 18 21 15
# provide an external function (here the weighted median function from the spatstat package)
#score_scale(dat, scale == "rel", label = "Religious beliefs", fun = spatstat.geom::weighted.median)
```
