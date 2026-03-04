# Remove dictionary class and information from a data.frame or a vector

Remove dictionary class and information from a data.frame or a vector

## Usage

``` r
remove_dic_class(data, remove_attributes = FALSE)
```

## Arguments

- data:

  A vector or a data.frame (or an object that inherits from a
  data.frame)

- remove_attributes:

  If TRUE, all dictionary attributes are removed.

## Value

An object not inheriting from class "dic".

## Examples

``` r
dat <- remove_dic_class(ex_itrf)
class(ex_itrf$itrf_I_1)
#> [1] "dic"     "numeric"
class(dat$itrf_I_1)
#> [1] "numeric"
```
