# (Deprecated) Rename items

Rename items based on dic information.

## Usage

``` r
rename_items_deprecated(
  data,
  pattern = "item_label",
  chars = NULL,
  char_sep = "_",
  char_weight = c("(-)", "(+)"),
  char_prefix_label = ": "
)
```

## Arguments

- data:

  A data frame

- pattern:

  A character string or vector of character strings defining a prefix.
  May include the name of any dic attribute (e.g."item_label", "scale",
  "subscale", "subscale_2") or some shortcuts: "reverse", "label", or
  "name").

- chars:

  If not NULL, only the first n chars og the long label will be applied.

- char_sep:

  Character with separator between prefix information.

- char_weight:

  Character vector of length two with signs for negative and positive
  weights.

- char_prefix_label:

  deprecated

## Value

A renamed data frame

## Examples

``` r
ex_itrf  |>
  rename_items(pattern = c("reverse", "label")) |>
  names()
#> Warning: The pattern definition is deprecated. Please use glue::glue style syntax to defnine pattern. E.g. '{name}: {label}'
#>  [1] "(+): Verbringt zu viel Zeit alleine"                                                                      
#>  [2] "(+): Beschwert sich über Krankheit oder Schmerzen"                                                        
#>  [3] "(+): Vermeidet soziale Interaktionen"                                                                     
#>  [4] "(+): Spielt bevorzugt alleine"                                                                            
#>  [5] "(+): Geht nicht auf Kontaktversuche der Mitschülerinnen und Mitschüler ein"                               
#>  [6] "(+): Macht sich Sorgen über unwichtige Details"                                                           
#>  [7] "(+): Beschwert sich über Kopfschmerzen oder Bauchschmerzen"                                               
#>  [8] "(+): Wirkt unglücklich oder traurig"                                                                      
#>  [9] "(+): Klammert sich an Erwachsene"                                                                         
#> [10] "(+): Verhält sich nervös"                                                                                 
#> [11] "(+): Verhält sich ängstlich"                                                                              
#> [12] "(+): Behauptet sich nicht gegenüber anderen"                                                              
#> [13] "(+): Verhält sich übermäßig schüchtern"                                                                   
#> [14] "(+): Beklagt sich oder jammert"                                                                           
#> [15] "(+): Beteiligt sich nicht an Gruppenaktionen"                                                             
#> [16] "(+): Macht sich selbst schlecht"                                                                          
#> [17] "(+): Weint oder ist weinerlich"                                                                           
#> [18] "(+): Ist schnell verärgert"                                                                               
#> [19] "(+): Macht sich ständig Sorgen"                                                                           
#> [20] "(+): Lässt sich langsam auf neue Personen ein"                                                            
#> [21] "(+): Stellt Unterrichtsaufgaben nicht rechtzeitig fertig"                                                 
#> [22] "(+): Beginnt mit der Aufgabenbearbeitung nicht selbstständig"                                             
#> [23] "(+): Erledigt Hausaufgaben unvollständig"                                                                 
#> [24] "(+): Zeigt Unterrichtsaufgaben nicht selbstständig vor"                                                   
#> [25] "(+): Kontrolliert seine eigene Arbeit nicht"                                                              
#> [26] "(+): Nimmt Materialien, die zu Hause benötigt werden, nicht mit (z.B. für die Hausaufgaben, Elternbriefe)"
#> [27] "(+): Streitet und zankt mit Lehrkräften"                                                                  
#> [28] "(+): Verliert die Beherrschung"                                                                           
#> [29] "(+): Stört andere"                                                                                        
#> [30] "(+): Verwendet unangemessene Sprache"                                                                     
#> [31] "(+): Hat Konflikte mit Mitschülerinnen und Mitschülern"                                                   
#> [32] "(+): Kommandiert rum"                                                                                     
#> [33] "(+): Macht unangebrachte Bemerkungen"                                                                     
#> [34] "(+): Kommt unvorbereitet zum Unterricht"                                                                  
#> [35] "(+): Beteiligt sich nicht am Unterricht"                                                                  
#> [36] "(+): Respektiert nicht die Privatsphäre anderer"                                                          
#> [37] ": ITRF Internalizing"                                                                                     
#> [38] ": ITRF Externalizing"                                                                                     
```
