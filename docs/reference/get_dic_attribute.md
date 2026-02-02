# Get a dictionary attribute of one or more variables.

Retrieves a specific dic attribute from all variables in a data frame.

## Usage

``` r
get_dic_attribute(data, attribute, duplicates = TRUE)
```

## Arguments

- data:

  A data frame with dic information.

- attribute:

  Name of label attribute to retrieve.

- duplicates:

  If FALSE, duplicated attributes will be removed from returned vector.

## Value

Vector of attributes. If an attribute is not present, NA is returned.

## Details

If an attribute is not present for a variable, NA is returned for that
variable. If duplicates is set to FALSE, only unique attribute values
are returned.

## Examples

``` r
ex_itrf |>
  select_items(subscale == "Int") |>
  get_dic_attribute("item_label")
#>  [1] "Verbringt zu viel Zeit alleine"                                       
#>  [2] "Beschwert sich über Krankheit oder Schmerzen"                         
#>  [3] "Vermeidet soziale Interaktionen"                                      
#>  [4] "Spielt bevorzugt alleine"                                             
#>  [5] "Geht nicht auf Kontaktversuche der Mitschülerinnen und Mitschüler ein"
#>  [6] "Macht sich Sorgen über unwichtige Details"                            
#>  [7] "Beschwert sich über Kopfschmerzen oder Bauchschmerzen"                
#>  [8] "Wirkt unglücklich oder traurig"                                       
#>  [9] "Klammert sich an Erwachsene"                                          
#> [10] "Verhält sich nervös"                                                  
#> [11] "Verhält sich ängstlich"                                               
#> [12] "Behauptet sich nicht gegenüber anderen"                               
#> [13] "Verhält sich übermäßig schüchtern"                                    
#> [14] "Beklagt sich oder jammert"                                            
#> [15] "Beteiligt sich nicht an Gruppenaktionen"                              
#> [16] "Macht sich selbst schlecht"                                           
#> [17] "Weint oder ist weinerlich"                                            
#> [18] "Macht sich ständig Sorgen"                                            
#> [19] "Lässt sich langsam auf neue Personen ein"                             
```
