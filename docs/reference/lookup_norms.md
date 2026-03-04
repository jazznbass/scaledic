# Look up norm table values based on raw scores and group affiliations

Transforms raw values to norm values based on a norm table. If group
affiliations are provided, these are used to identify the correct norm
values. If no group affiliations are provided, norm values are looked up
based on raw scores only. In case of ambiguities (e.g., multiple norm
values for the same raw score), NA is returned and a message is printed.

## Usage

``` r
lookup_norms(
  rawscores,
  group = NULL,
  normtable,
  from = "raw",
  to = "T",
  group_label = names(group),
  label = NULL
)
```

## Arguments

- rawscores:

  A vector with raw scores.

- group:

  A vector with group affiliations or a list with vectors for multiple
  group categorizations. If NULL (default), no group affiliations are
  used.

- normtable:

  An excel file name or a data frame containing a norm table.

- from:

  Label of the raw score variable in file.

- to:

  Label of the norm score variable in file.

- group_label:

  Label of the group variable in file or a list with group labels for
  multiple group categorizations. If NULL (default), "group" is used as
  group label.

- label:

  Item label of the resulting variable. If NULL (default), no label is
  set.

## Value

A vector with norm values. If ambiguities occur, NA is returned for the
respective raw score(s) and a message is printed.

## Examples

``` r
normtable <- data.frame(
  age = rep(c(6, 8, 6, 8), each = 11),
  gender = rep(c("m", "w"), each = 22),
  raw =  rep(0:10, 4),
  T = rep(c(40, 42, 44, 46, 48, 50, 52, 54, 56, 58, 60,
            37, 39, 41, 43, 45, 47, 49, 51, 53, 55, 57), 2) + rep(c(0,5), each = 22)
)
rawscores <- c(5,5,3,1)
group_age <- c("6", "8", "6", "8")
group_gender <- c("m", "m", "w", "w")

lookup_norms(rawscores, group = list(age = group_age, gender = group_gender), normtable)
#> ║T_score 
#> ║Data type is numeric
#> ║ 
#> ║Length is 4 (0 NA; 0 invalid)
#> ║[1] 50 47 51 44

## When group values are not specified exactly, raw scores can be ambiguous:
lookup_norms(rawscores, group = list(gender = group_gender), normtable = normtable)
#> ! Multiple values found for raw 5 and group m (50, 47). NA returned.
#> ! Multiple values found for raw 5 and group m (50, 47). NA returned.
#> ! Multiple values found for raw 3 and group w (51, 48). NA returned.
#> ! Multiple values found for raw 1 and group w (47, 44). NA returned.
#> ! Type of T_score is missing and is estimated as 'logical'.
#> ║T_score 
#> ║Data type is logical
#> ║ 
#> ║Length is 4 (4 NA; 0 invalid)
#> ║[1] NA NA NA NA

lookup_norms(rawscores, normtable = normtable)
#> ! Multiple values found for raw 5 (50, 47, 55, 52). NA returned.
#> ! Multiple values found for raw 5 (50, 47, 55, 52). NA returned.
#> ! Multiple values found for raw 3 (46, 43, 51, 48). NA returned.
#> ! Multiple values found for raw 1 (42, 39, 47, 44). NA returned.
#> ! Type of T_score is missing and is estimated as 'logical'.
#> ║T_score 
#> ║Data type is logical
#> ║ 
#> ║Length is 4 (4 NA; 0 invalid)
#> ║[1] NA NA NA NA
```
