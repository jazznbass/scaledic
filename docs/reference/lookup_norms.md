# Look up norm table values

Transforms raw values to norm values based on a norm table.

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
  group categorizations.

- normtable:

  An excel file name or a data frame containing a norm table.

- from:

  Label of the raw score variable in file.

- to:

  Label of the norm score variable in file.

- group_label:

  Label of the group variable in file or a list with group labels for
  multiple group categorizations.

- label:

  Item label of the resulting variable

## Value

A vector with norm values.

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
#> [1] 50 47 51 44

## When group values are not specified exactly, raw scores can be ambiguous:
lookup_norms(rawscores, group = list(gender = group_gender), normtable = normtable)
#> ! (                " and group ", y, " (", paste0((normtable[[to]][id]), )
#> 1:             add_message("Multiple values found for raw ", x,  (4x)
#> ! (            return(NA))
#> 1:                 ")", ". NA returned.") (4x)
#> ! (        id <- which(normtable[[from]] == x & normtable[[group_label]] == )
#> 1:     else { (4x)
#> ! (        if (length(id) > 1) {)
#> 1:         id <- which(normtable[[from]] == x) (4x)
#> ! (    })
#> 1:         } (4x)
#> ! (})
#> 1:     normtable[[to]][id] (4x)
#> [1] NA NA NA NA

lookup_norms(rawscores, normtable = normtable)
#> ! (                " and group ", y, " (", paste0((normtable[[to]][id]), )
#> 1:             add_message("Multiple values found for raw ", x,  (4x)
#> ! (            return(NA))
#> 1:                 ")", ". NA returned.") (4x)
#> ! (        id <- which(normtable[[from]] == x & normtable[[group_label]] == )
#> 1:     else { (4x)
#> ! (        if (length(id) > 1) {)
#> 1:         id <- which(normtable[[from]] == x) (4x)
#> ! (    })
#> 1:         } (4x)
#> ! (})
#> 1:     normtable[[to]][id] (4x)
#> [1] NA NA NA NA
```
