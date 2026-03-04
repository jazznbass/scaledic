# Score scale calculates scale scores

Calculates scale scores based on item variables defined in the
dictionary. Uses a weighted mean (default) or weighted sum function to
calculate the scores. Allows for filtering items based on any *dic*
attribute (e.g., `scale`, `subscale`, etc.). Also allows for providing a
custom function for calculating the scores.

## Usage

``` r
score_scale(
  data,
  filter = NULL,
  scales = NULL,
  fun = "mean",
  min_valid = 1,
  max_na = NA,
  label = NULL,
  sum = NULL,
  var_recoding = "recodes",
  use_weights = TRUE
)
```

## Arguments

- data:

  A data frame.

- filter:

  A logical expression for any *dic* attribute (e.g.
  `scale == "ITRF" & subscale == "Int"`).

- scales:

  Alternatively, a named list with scale definition created with the
  [`get_scales()`](get_scales.md) function. Not used when `filter` is
  provided.

- fun:

  A function or a string with the name of a predefined function that is
  applied to calculate the scale score. The function should take a
  data.frame of item values and a numeric vector of weights as
  arguments. The function should return a single numeric value
  representing the calculated score. Predefined functions are "mean"
  (weighted mean), "sum" (weighted sum), "fa_score" (factor scores based
  on a factor analysis with one factor), and "pc_score" (scores based on
  the first principal component).

- min_valid:

  Minimal number of valid values of each case. The score of a case will
  be set to NA if the number of valid values is below this threshold. A
  value between 0 and 1 indicates a proportion of values (e.g., 0.5 = 50
  percent at least have to be valid).

- max_na:

  Maximum number of NA values of each case. The score of a case will be
  set to NA if the number of valid values is above this threshold. A
  value between 0 and 1 indicates a proportion of values (e.g., 0.5 = 50
  percent at most NAs are allowed).

- label:

  A character string with a label for the resulting score variable.
  Automatically generated if label is not set.

- sum:

  (Deprecated) If `FALSE`, a weighted mean function is applied for
  building the scores. If `TRUE`, a weighted sum function is applied.
  When argument fun is set, `sum` is ignored.

- var_recoding:

  Name if the *dic* attribute that may contain recoding information.
  Defaults to `scores`.

- use_weights:

  If `TRUE`, weights are applied when calculating the scores.

## Value

A data frame with the calculated scale score. If only the filter
argument is provided, a vector with the calculated scale score is
returned.

## Details

If the filter argument is provided, scale scores are calculated based on
that filter. If the scales argument is provided, scale scores are
calculated for each scale defined in that list. The default function for
calculating the scale score is "mean", a weighted mean function.

## See also

[`get_scales`](get_scales.md), [`apply_dic`](apply_dic.md),
[`dic_attr`](dic_attr.md)

## Author

Jürgen Wilbert

## Examples

``` r
dat <- apply_dic(ex_scaledic_data, ex_scaledic_dic)
#> ! Set 1 NA in factor 'gender' for value -999
#> ! Replaced 1 missing value in 'rel_3' with NA
#> ! Replaced 1 missing value in 'rel_4' with NA
#> ! Replaced 1 missing value in 'sui_2' with NA
#> ! Replaced 1 missing value in 'age' with NA
# apply the default weighted mean function
score_scale(dat, scale == "rel", label = "Religious beliefs")
#> ╭─ scoring 'Religious beliefs' with 'mean'
#> ℹ Scores calculated for 20 cases (100%)
#> ║Religious beliefs
#> ║Scored scale: "mean" of items: scale == "rel" 
#> ║Data type is numeric
#> ║ 
#> ║Length is 20 (0 NA; 0 invalid)
#> ║ [1]  2.20  1.60 15.20  2.60  3.80  3.60  2.40  3.40  4.40  4.80 15.40  3.40
#> ║[13]  3.80  2.25  2.60  3.80  3.80  4.50  4.20  3.00
# apply the weighted sum function
score_scale(dat, scale == "rel", label = "Religious beliefs", fun = "sum")
#> ╭─ scoring 'Religious beliefs' with 'sum'
#> ! Scores for 2 cases with missing values are scaled to the full number of items.
#> ℹ Scores calculated for 20 cases (100%)
#> ║Religious beliefs
#> ║Scored scale: "sum" of items: scale == "rel" 
#> ║Data type is numeric
#> ║ 
#> ║Length is 20 (0 NA; 0 invalid)
#> ║ [1] 11.00  8.00 76.00 13.00 19.00 18.00 12.00 17.00 22.00 24.00 77.00 17.00
#> ║[13] 19.00 11.25 13.00 19.00 19.00 22.50 21.00 15.00
```
