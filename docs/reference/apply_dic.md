# Apply dictionary to data frame

Joins a data frame with a dictionary file.

## Usage

``` r
apply_dic(
  data,
  dic,
  set_label_attr = TRUE,
  coerce_class = TRUE,
  replace_missing = TRUE,
  score_scales = TRUE,
  check_values = FALSE,
  impute_values = FALSE,
  rename_var = NULL,
  format_date = "%Y-%m-%d"
)
```

## Arguments

- data:

  Data frame or a character string with a filename (for now an Microsoft
  Excel file) containing the data.

- dic:

  A data frame comprising a dictionary or a character string with a
  filename (for now an Microsoft Excel file) containing a dictionary.

- set_label_attr:

  If TRUE, label attributes from the haven package are set. These labels
  are shown in the Rstudio View panel.

- coerce_class:

  If set TRUE mismatches between class and dic type are corrected.

- replace_missing:

  If TRUE, missing values from the dic are replaced with NAs in the data
  frame.

- score_scales:

  If TRUE and the dic files contains score scale definitions these are
  applied to the data frame.

- check_values:

  If TRUE, performs the check_values function on the variables of the
  data frame included in the dic file.

- impute_values:

  If TRUE and score_scales is TRUE, missing values are automatically
  imputed based on scale information provided in the dic file.

- rename_var:

  When a character is provided, corresponding column from the dic-file
  is used to rename variables from 'rename_var' (old) to 'item_name'
  (new) in the `data` data frame.

- format_date:

  Optional string that is applied when character variable is coerced to
  a 'Date' class.

## Value

A data frame with dictionary information.

## See also

[`new_dic`](new_dic.md), [`replace_missing`](replace_missing.md),
[`score_from_dic`](score_from_dic.md), `check_values `,
[`dic_haven`](dic_haven.md)

## Author

Jürgen Wilbert

## Examples

``` r
dat <- apply_dic(dat_itrf, dic_itrf)
#> 41 messages generated (type show_messages() to see details).
descriptives(dat)
#>         name valid missing mean   sd min  max range median  mad
#> 1   itrf_I_1  4247     529 0.38 0.72   0 3.00  3.00   0.00 0.00
#> 2   itrf_I_2  4757      19 0.35 0.69   0 3.00  3.00   0.00 0.00
#> 3   itrf_I_4  4758      18 0.31 0.63   0 3.00  3.00   0.00 0.00
#> 4   itrf_I_5  4755      21 0.24 0.59   0 3.00  3.00   0.00 0.00
#> 5   itrf_I_6  4754      22 0.21 0.51   0 3.00  3.00   0.00 0.00
#> 6   itrf_I_7  4751      25 0.41 0.72   0 3.00  3.00   0.00 0.00
#> 7   itrf_I_8  4753      23 0.35 0.69   0 3.00  3.00   0.00 0.00
#> 8   itrf_I_9  4761      15 0.48 0.77   0 3.00  3.00   0.00 0.00
#> 9  itrf_I_10  4761      15 0.28 0.66   0 3.00  3.00   0.00 0.00
#> 10 itrf_I_11  4757      19 0.33 0.68   0 3.00  3.00   0.00 0.00
#> 11 itrf_I_12  4755      21 0.37 0.69   0 3.00  3.00   0.00 0.00
#> 12 itrf_I_13  4754      22 0.42 0.72   0 3.00  3.00   0.00 0.00
#> 13 itrf_I_14  4757      19 0.35 0.69   0 3.00  3.00   0.00 0.00
#> 14 itrf_I_15  4755      21 0.37 0.71   0 3.00  3.00   0.00 0.00
#> 15 itrf_I_16  4755      21 0.43 0.76   0 3.00  3.00   0.00 0.00
#> 16 itrf_I_17  4757      19 0.38 0.71   0 3.00  3.00   0.00 0.00
#> 17 itrf_I_19  4761      15 0.23 0.59   0 3.00  3.00   0.00 0.00
#> 18 itrf_I_20  4753      23 0.55 0.91   0 3.00  3.00   0.00 0.00
#> 19 itrf_I_23  4741      35 0.36 0.68   0 3.00  3.00   0.00 0.00
#> 20 itrf_I_24  4747      29 0.38 0.71   0 3.00  3.00   0.00 0.00
#> 21  itrf_E_1  4757      19 0.95 1.04   0 3.00  3.00   1.00 1.48
#> 22  itrf_E_2  4754      22 0.75 0.97   0 3.00  3.00   0.00 0.00
#> 23  itrf_E_3  4616     160 0.57 0.90   0 3.00  3.00   0.00 0.00
#> 24  itrf_E_4  4702      74 0.57 0.86   0 3.00  3.00   0.00 0.00
#> 25  itrf_E_5  4729      47 0.93 0.96   0 3.00  3.00   1.00 1.48
#> 26  itrf_E_6  4751      25 0.53 0.87   0 3.00  3.00   0.00 0.00
#> 27  itrf_E_7  4755      21 0.35 0.75   0 3.00  3.00   0.00 0.00
#> 28  itrf_E_8  4757      19 0.49 0.90   0 3.00  3.00   0.00 0.00
#> 29  itrf_E_9  4756      20 0.83 0.96   0 3.00  3.00   1.00 1.48
#> 30 itrf_E_10  4756      20 0.40 0.79   0 3.00  3.00   0.00 0.00
#> 31 itrf_E_11  4754      22 0.77 0.95   0 3.00  3.00   0.00 0.00
#> 32 itrf_E_12  4753      23 0.46 0.81   0 3.00  3.00   0.00 0.00
#> 33 itrf_E_13  4751      25 0.47 0.85   0 3.00  3.00   0.00 0.00
#> 34 itrf_E_14  4723      53 0.38 0.75   0 3.00  3.00   0.00 0.00
#> 35 itrf_E_15  4749      27 0.70 0.91   0 3.00  3.00   0.00 0.00
#> 36 itrf_E_16  4761      15 0.35 0.72   0 3.00  3.00   0.00 0.00
#> 37  itrf_int  4772       4 0.35 0.42   0 2.63  2.63   0.21 0.31
#> 38  itrf_ext  4776       0 0.59 0.60   0 3.00  3.00   0.41 0.52
```
