# Enriches a data frame with dic information

Adds dic information to all variables in a data frame that do not have a
dic attribute. This is useful when creating a dic from a data frame that
has some variables already enriched with dic information and some that
are not. The function uses the variable name as item_name and the
variable label (if present) or the variable name as item_label.

## Usage

``` r
enrich_dic(dat)
```

## Arguments

- dat:

  A data frame.

## Value

A data frame with dic information added to all variables.

## Details

This function loops through all variables in the data frame and checks
if they have a dic attribute. If not, it adds a dic attribute using the
variable name and label.

## Examples

``` r
df <- data.frame(
  age = c(25, 30, 35),
  gender = c("M", "F", "M")
)
df$age <- dic(df$age, item_name = "age", item_label = "Age of respondent")
#> ! Type of age is missing and is estimated as 'numeric'.
df_enriched <- enrich_dic(df)
#> ! Type of gender is missing and is estimated as 'character'.
attributes(df_enriched$gender)$dic |> str()
#> List of 6
#>  $ item_name : chr "gender"
#>  $ item_label: chr "gender"
#>  $ weight    : num 1
#>  $ type      : chr "character"
#>  $ class     : chr "item"
#>  $ recodes   : NULL
```
