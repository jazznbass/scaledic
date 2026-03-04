# Exploratory factor analysis based on the psych::fa function

Creates a data frame with factor loadings and variance explained.

## Usage

``` r
exploratory_fa(..., factor_names = NULL, sort = TRUE, cut = 0.2, round = 2)
```

## Arguments

- ...:

  Arguments passed to the psych::fa function.

- factor_names:

  A character vector with names for the resulting factors. If not
  provided, default names are chosen.

- sort:

  If TRUE, loadings are sorted.

- cut:

  Loadings below cut will be omitted in the resulting data frame.

- round:

  Number of digits to round loadings (based on the base::round
  function).

## Value

A data.frame.

## See also

[`fa`](https://rdrr.io/pkg/psych/man/fa.html)

## Author

Jürgen Wilbert

## Examples

``` r
## Without factor names
ex_itrf  |>
  select_items(subscale %in% c('Int', 'Ext'))  |>
  exploratory_fa(nfactors = 2)
#> Loading required namespace: GPArotation
#>                         MR1  MR2
#> itrf_E_13              0.82     
#> itrf_E_9               0.81     
#> itrf_E_11               0.8     
#> itrf_E_10              0.79     
#> itrf_E_8               0.79     
#> itrf_E_7               0.78     
#> itrf_I_20              0.73     
#> itrf_E_12              0.68     
#> itrf_E_16              0.67     
#> itrf_E_14              0.56     
#> itrf_E_6               0.53     
#> itrf_E_5               0.52 0.24
#> itrf_E_3               0.51     
#> itrf_E_2               0.49 0.28
#> itrf_E_4               0.48 0.26
#> itrf_E_1               0.42 0.27
#> itrf_I_4                    0.74
#> itrf_I_12                    0.7
#> itrf_I_13             -0.21 0.68
#> itrf_I_14              -0.3 0.68
#> itrf_I_5                    0.68
#> itrf_I_1                    0.66
#> itrf_I_6                    0.66
#> itrf_I_16                   0.64
#> itrf_I_9                0.2 0.62
#> itrf_I_24                   0.55
#> itrf_I_23                   0.53
#> itrf_I_7                    0.52
#> itrf_I_10                   0.51
#> itrf_I_19                   0.48
#> itrf_E_15                   0.43
#> itrf_I_8                    0.43
#> itrf_I_2                    0.43
#> itrf_I_15              0.34 0.41
#> itrf_I_11              0.26 0.41
#> itrf_I_17              0.32  0.4
#> SS loadings            7.97 7.14
#> Proportion Var         0.22  0.2
#> Cumulative Var         0.22 0.42
#> Proportion Explained   0.53 0.47
#> Cumulative Proportion  0.53    1
## With factor names
ex_itrf  |>
  select_items(subscale %in% c('Int', 'Ext'))  |>
  exploratory_fa(nfactors = 2, factor_names = c("Internalizing", "Externalizing"))
#>                       Internalizing Externalizing
#> itrf_E_13                      0.82              
#> itrf_E_9                       0.81              
#> itrf_E_11                       0.8              
#> itrf_E_10                      0.79              
#> itrf_E_8                       0.79              
#> itrf_E_7                       0.78              
#> itrf_I_20                      0.73              
#> itrf_E_12                      0.68              
#> itrf_E_16                      0.67              
#> itrf_E_14                      0.56              
#> itrf_E_6                       0.53              
#> itrf_E_5                       0.52          0.24
#> itrf_E_3                       0.51              
#> itrf_E_2                       0.49          0.28
#> itrf_E_4                       0.48          0.26
#> itrf_E_1                       0.42          0.27
#> itrf_I_4                                     0.74
#> itrf_I_12                                     0.7
#> itrf_I_13                     -0.21          0.68
#> itrf_I_14                      -0.3          0.68
#> itrf_I_5                                     0.68
#> itrf_I_1                                     0.66
#> itrf_I_6                                     0.66
#> itrf_I_16                                    0.64
#> itrf_I_9                        0.2          0.62
#> itrf_I_24                                    0.55
#> itrf_I_23                                    0.53
#> itrf_I_7                                     0.52
#> itrf_I_10                                    0.51
#> itrf_I_19                                    0.48
#> itrf_E_15                                    0.43
#> itrf_I_8                                     0.43
#> itrf_I_2                                     0.43
#> itrf_I_15                      0.34          0.41
#> itrf_I_11                      0.26          0.41
#> itrf_I_17                      0.32           0.4
#> SS loadings                    7.97          7.14
#> Proportion Var                 0.22           0.2
#> Cumulative Var                 0.22          0.42
#> Proportion Explained           0.53          0.47
#> Cumulative Proportion          0.53             1
```
