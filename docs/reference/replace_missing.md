# Replace Missing values

This function replaces missing values in a data frame based on the
missing value definitions provided in the dic attributes of the
variables. Variables without dic attributes are ignored.

## Usage

``` r
replace_missing(data, replace = NA, report = TRUE)
```

## Arguments

- data:

  A data frame

- replace:

  Replace value for missing values (default is NA).

- report:

  If TRUE, proportion of missing values is printed.

## Value

A data frame with replaced missing values.
