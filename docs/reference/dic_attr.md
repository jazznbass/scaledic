# Set and get dictionary attributes

Get or set dictionary attributes of a vector or a data.frame.

## Usage

``` r
dic_attr(x, var)

dic_attr(x, var) <- value
```

## Arguments

- x:

  A vector or a data.frame.

- var:

  Attribute name. If missing, the full attribute list is returned.

- value:

  set value.

## Value

Attribute value

Modified object with updated attribute.

## Examples

``` r
# set dic attributes
x <- 1:5
dic_attr(x, "class") <- "item"
dic_attr(x, "item_label") <- "An example item"
dic_attr(x, "item_label")
#> [1] "An example item"
dic_attr(x)
#> $class
#> [1] "item"
#> 
#> $item_label
#> [1] "An example item"
#> 
```
