# Check values in data frame according to dic attributes

Checks if values in variables are valid according to the 'values' and
'type' dictionary attributes. Invalid values can be replaced with a
specified value. An overview of invalid values can be reported.

## Usage

``` r
check_values(
  data,
  replace = NULL,
  return = TRUE,
  report = TRUE,
  include_missing = FALSE,
  integer_as_numeric = TRUE
)
```

## Arguments

- data:

  A data frame.

- replace:

  Value which replaces invalid values (e.g., NA). If NULL, no
  replacement is done.

- return:

  If TRUE, a data frame is returned with replaced values. If FALSE, no
  data frame is returned.

- report:

  If TRUE, an overview of invalid values will be given.

- include_missing:

  If TRUE, missing values (provided as 'missing' in the dic file) will
  be considered as valid values. If FALSE, missing values will be
  considered as invalid values.

- integer_as_numeric:

  If TRUE, type 'integer' will be handled as 'numeric'. That is, only
  values outside the minimum and the maximum of the provided valid
  values will be considered invalid. If FALSE, only values not included
  in the provided valid values will be considered invalid.

## Value

A data frame with replaced values if `replaces` is not NULL.

## Details

This function only checks variables that have dic attributes. Variables
without dic attributes are ignored.

By default, integer variables are treated as float variables. That is,
only values outside the provided valid values are considered invalid. If
`integer_as_float` is set to FALSE, only values not included in the
provided valid values are considered invalid.

If `include_missing` is set to TRUE, values provided as 'missing' in the
dic file are considered valid values.

## Examples

``` r
check_values(ex_itrf, return = FALSE)
#> ! No errors found.
#>   
```
