# Extract a dictionary from a data file with dic information

This function extracts the dictionary information from a data frame
containing dic attributes and returns it as a data frame in a dictionary
format.

## Usage

``` r
extract_dic(data)
```

## Arguments

- data:

  A data frame with dic information.

## Value

A data frame in a dictionary format.

## Examples

``` r
## extract dic from ex_itrf
ex_itrf_dic <- extract_dic(ex_itrf)
head(ex_itrf_dic)
#>   item_name
#> 1  itrf_I_1
#> 2  itrf_I_2
#> 3  itrf_I_4
#> 4  itrf_I_5
#> 5  itrf_I_6
#> 6  itrf_I_7
#>                                                              item_label values
#> 1                                        Verbringt zu viel Zeit alleine    0:3
#> 2                          Beschwert sich über Krankheit oder Schmerzen    0:3
#> 3                                       Vermeidet soziale Interaktionen    0:3
#> 4                                              Spielt bevorzugt alleine    0:3
#> 5 Geht nicht auf Kontaktversuche der Mitschülerinnen und Mitschüler ein    0:3
#> 6                             Macht sich Sorgen über unwichtige Details    0:3
#>                                                                               value_labels
#> 1 0 = not problematic; 1 = slightly problematic; 2 = problematic; 3 = strongly problematic
#> 2 0 = not problematic; 1 = slightly problematic; 2 = problematic; 3 = strongly problematic
#> 3 0 = not problematic; 1 = slightly problematic; 2 = problematic; 3 = strongly problematic
#> 4 0 = not problematic; 1 = slightly problematic; 2 = problematic; 3 = strongly problematic
#> 5 0 = not problematic; 1 = slightly problematic; 2 = problematic; 3 = strongly problematic
#> 6 0 = not problematic; 1 = slightly problematic; 2 = problematic; 3 = strongly problematic
#>   missing weight scale subscale subscale_2                    scale_label
#> 1     -99      1  ITRF      Int         SW Integrated teacher report form
#> 2     -99      1  ITRF      Int         AD Integrated teacher report form
#> 3     -99      1  ITRF      Int         SW Integrated teacher report form
#> 4     -99      1  ITRF      Int         SW Integrated teacher report form
#> 5     -99      1  ITRF      Int         SW Integrated teacher report form
#> 6     -99      1  ITRF      Int         AD Integrated teacher report form
#>   subscale_label   subscale_2_label    type                            source
#> 1  Internalizing Socially Withdrawn integer Volpe et al. et al. (unpublished)
#> 2  Internalizing  Anxious/Depressed integer Volpe et al. et al. (unpublished)
#> 3  Internalizing Socially Withdrawn integer Volpe et al. et al. (unpublished)
#> 4  Internalizing Socially Withdrawn integer Volpe et al. et al. (unpublished)
#> 5  Internalizing Socially Withdrawn integer Volpe et al. et al. (unpublished)
#> 6  Internalizing  Anxious/Depressed integer Volpe et al. et al. (unpublished)
#>   class score_filter score_function index note
#> 1  item         <NA>           <NA>     1 <NA>
#> 2  item         <NA>           <NA>     2 <NA>
#> 3  item         <NA>           <NA>     4 <NA>
#> 4  item         <NA>           <NA>     5 <NA>
#> 5  item         <NA>           <NA>     6 <NA>
#> 6  item         <NA>           <NA>     7 <NA>
```
