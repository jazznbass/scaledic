# Check values

Checks if values in variables are valid according to the 'values' and
'type' dictionary attributes. Invalid values can be replaced with a
specified value.

## Usage

``` r
check_values(
  data,
  replace = NULL,
  return = TRUE,
  report = TRUE,
  include_missing = FALSE,
  integer_as_float = FALSE,
  check_type = TRUE
)
```

## Arguments

- data:

  A data frame.

- replace:

  Value which replaces invalid values (e.g., NA).

- return:

  If TRUE, a data frame is returned with replaced values.

- report:

  If TRUE, an overview of invalid values will be given.

- include_missing:

  If TRUE, missing values (provided as 'missing' in the dic file) will
  be considered as valid values.

- integer_as_float:

  If TRUE, type 'integer' will be handled as 'float'. That is, only
  values outside the minimum and the maximum of the provided valid
  values will be considered invalid.

## Value

A data frame with replaced values if `replaces` is not NULL.

## Examples

``` r
check_values(ex_itrf, return = FALSE)
#> ! (check_values)
#> 1: No errors found.
#> 
```
