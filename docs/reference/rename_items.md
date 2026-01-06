# Rename items

Rename items based on dic information.

## Usage

``` r
rename_items(
  data,
  pattern = "{item_label}",
  max_chars = NULL,
  chars = max_chars
)
```

## Arguments

- data:

  A data frame

- pattern:

  A character string with the syntax of the glue function (see
  [`glue::glue()`](https://glue.tidyverse.org/reference/glue.html)).

- max_chars, chars:

  If not NULL, only the first n chars of the resulting label will be
  applied.

## Value

A renamed data frame

## Examples

``` r
ex_itrf  |>
  rename_items(pattern = "{reverse}{name}: {label}")  |>
  names()
#>  [1] "+itrf_I_1: Verbringt zu viel Zeit alleine"                                                                      
#>  [2] "+itrf_I_2: Beschwert sich über Krankheit oder Schmerzen"                                                        
#>  [3] "+itrf_I_4: Vermeidet soziale Interaktionen"                                                                     
#>  [4] "+itrf_I_5: Spielt bevorzugt alleine"                                                                            
#>  [5] "+itrf_I_6: Geht nicht auf Kontaktversuche der Mitschülerinnen und Mitschüler ein"                               
#>  [6] "+itrf_I_7: Macht sich Sorgen über unwichtige Details"                                                           
#>  [7] "+itrf_I_8: Beschwert sich über Kopfschmerzen oder Bauchschmerzen"                                               
#>  [8] "+itrf_I_9: Wirkt unglücklich oder traurig"                                                                      
#>  [9] "+itrf_I_10: Klammert sich an Erwachsene"                                                                        
#> [10] "+itrf_I_11: Verhält sich nervös"                                                                                
#> [11] "+itrf_I_12: Verhält sich ängstlich"                                                                             
#> [12] "+itrf_I_13: Behauptet sich nicht gegenüber anderen"                                                             
#> [13] "+itrf_I_14: Verhält sich übermäßig schüchtern"                                                                  
#> [14] "+itrf_I_15: Beklagt sich oder jammert"                                                                          
#> [15] "+itrf_I_16: Beteiligt sich nicht an Gruppenaktionen"                                                            
#> [16] "+itrf_I_17: Macht sich selbst schlecht"                                                                         
#> [17] "+itrf_I_19: Weint oder ist weinerlich"                                                                          
#> [18] "+itrf_I_20: Ist schnell verärgert"                                                                              
#> [19] "+itrf_I_23: Macht sich ständig Sorgen"                                                                          
#> [20] "+itrf_I_24: Lässt sich langsam auf neue Personen ein"                                                           
#> [21] "+itrf_E_1: Stellt Unterrichtsaufgaben nicht rechtzeitig fertig"                                                 
#> [22] "+itrf_E_2: Beginnt mit der Aufgabenbearbeitung nicht selbstständig"                                             
#> [23] "+itrf_E_3: Erledigt Hausaufgaben unvollständig"                                                                 
#> [24] "+itrf_E_4: Zeigt Unterrichtsaufgaben nicht selbstständig vor"                                                   
#> [25] "+itrf_E_5: Kontrolliert seine eigene Arbeit nicht"                                                              
#> [26] "+itrf_E_6: Nimmt Materialien, die zu Hause benötigt werden, nicht mit (z.B. für die Hausaufgaben, Elternbriefe)"
#> [27] "+itrf_E_7: Streitet und zankt mit Lehrkräften"                                                                  
#> [28] "+itrf_E_8: Verliert die Beherrschung"                                                                           
#> [29] "+itrf_E_9: Stört andere"                                                                                        
#> [30] "+itrf_E_10: Verwendet unangemessene Sprache"                                                                    
#> [31] "+itrf_E_11: Hat Konflikte mit Mitschülerinnen und Mitschülern"                                                  
#> [32] "+itrf_E_12: Kommandiert rum"                                                                                    
#> [33] "+itrf_E_13: Macht unangebrachte Bemerkungen"                                                                    
#> [34] "+itrf_E_14: Kommt unvorbereitet zum Unterricht"                                                                 
#> [35] "+itrf_E_15: Beteiligt sich nicht am Unterricht"                                                                 
#> [36] "+itrf_E_16: Respektiert nicht die Privatsphäre anderer"                                                         
#> [37] "ITRF Internalizing: ITRF Internalizing"                                                                         
#> [38] "ITRF Externalizing: ITRF Externalizing"                                                                         
```
