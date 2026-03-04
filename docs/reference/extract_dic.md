# Extract a dictionary from a data file with dic information

This function extracts the dictionary information from a data frame
containing dic attributes and returns it as a data frame in a dictionary
format. The resulting data frame contains one row per variable with dic
information and columns for each dic attribute, such as item_name,
item_label, values, value_labels, missing, recodes, and others. The
function handles different variable types and formats the values, value
labels, missing values, and recodes appropriately for inclusion in the
dictionary data frame.

## Usage

``` r
extract_dic(data)
```

## Arguments

- data:

  A data frame with dic information.

## Value

A data frame in a dictionary format.

## Details

The function first identifies the variables in the data frame that have
dic attributes. It then creates an empty data frame with columns for
each dic attribute and fills in the information for each variable.
Special handling is done for the values, value labels, missing values,
and recodes to ensure they are formatted correctly for the dictionary.
This function is useful for extracting and exporting the dic information
from a data frame for documentation or further processing.

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
#>   missing weight    type class recodes index scale subscale subscale_2
#> 1     -99      1 integer  item      NA     1  ITRF      Int         SW
#> 2     -99      1 integer  item      NA     2  ITRF      Int         AD
#> 3     -99      1 integer  item      NA     4  ITRF      Int         SW
#> 4     -99      1 integer  item      NA     5  ITRF      Int         SW
#> 5     -99      1 integer  item      NA     6  ITRF      Int         SW
#> 6     -99      1 integer  item      NA     7  ITRF      Int         AD
#>                      scale_label subscale_label   subscale_2_label
#> 1 Integrated teacher report form  Internalizing Socially Withdrawn
#> 2 Integrated teacher report form  Internalizing  Anxious/Depressed
#> 3 Integrated teacher report form  Internalizing Socially Withdrawn
#> 4 Integrated teacher report form  Internalizing Socially Withdrawn
#> 5 Integrated teacher report form  Internalizing Socially Withdrawn
#> 6 Integrated teacher report form  Internalizing  Anxious/Depressed
#>                              source note score_filter score_function
#> 1 Volpe et al. et al. (unpublished) <NA>         <NA>           <NA>
#> 2 Volpe et al. et al. (unpublished) <NA>         <NA>           <NA>
#> 3 Volpe et al. et al. (unpublished) <NA>         <NA>           <NA>
#> 4 Volpe et al. et al. (unpublished) <NA>         <NA>           <NA>
#> 5 Volpe et al. et al. (unpublished) <NA>         <NA>           <NA>
#> 6 Volpe et al. et al. (unpublished) <NA>         <NA>           <NA>
```
