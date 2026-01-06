# Rename variables based on a list

Rename the variable in a dataset based of a provided renaming list

## Usage

``` r
rename_by_list(data, file = NULL, to_from = NULL, to = NULL, from = NULL)
```

## Arguments

- data:

  A data frame

- file:

  A filename of an Excel file or a data.frame containing at least two
  columns for renaming (variables to rename and target names)

- to_from:

  When a filename or a data.frame is provided, a named character with
  the names of the target and source variable names (e.g., c("to" =
  "from")). When no filename is provided, to_from must be a vector with
  named variable names c("to1" = "from1", "to2" = "from2"))

- to:

  When a filename or a data.frame is provided, the name of the column
  with the target variable names. When no filename is provided, to must
  be a vector with target variable names

- from:

  When a filename or a data.frame is provided, the name of the column
  with the source variable names. When no filename is provided, from
  must a vector with source variable names

## Value

A data frame

## Examples

``` r
dat <- data.frame(A = NA, B = NA, C = NA, D = NA)
rename_by_list(dat, to = c("albert", "bea"), from = c("A", "B"))
#> 2 variables renamed
#>   albert bea  C  D
#> 1     NA  NA NA NA
rename_by_list(dat, to_from = c("carl" = "C", "daniel" = "D"))
#> 2 variables renamed
#>    A  B carl daniel
#> 1 NA NA   NA     NA
dic <- data.frame(old = c("A", "B"), new = c("albert", "bea"))
rename_by_list(dat, dic, to_from = c("new" = "old"))
#> 2 variables renamed
#>   albert bea  C  D
#> 1     NA  NA NA NA
rename_by_list(dat, dic, to = "new", from = "old")
#> 2 variables renamed
#>   albert bea  C  D
#> 1     NA  NA NA NA
if (FALSE) { # \dontrun{
rename_by_list(dat, "rename_list,xlsx", to_from = c("new" = "old"))
rename_by_list(dat, "rename_list,xlsx", to = "new", from = "old")
} # }
```
