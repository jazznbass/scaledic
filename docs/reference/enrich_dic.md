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

## Examples

``` r
df <- data.frame(
  age = c(25, 30, 35),
  gender = c("M", "F", "M")
)
df$age <- new_dic(df$age, item_name = "age", item_label = "Age of respondent")
#> Error in new_dic(df$age, item_name = "age", item_label = "Age of respondent"): unused arguments (item_name = "age", item_label = "Age of respondent")
df_enriched <- enrich_dic(df)
#> Error in new_dic(dat[[i]], item_name = item_name, item_label = item_label): unused arguments (item_name = item_name, item_label = item_label)
attributes(df_enriched$gender)$dic |> str()
#> Error: object 'df_enriched' not found
```
