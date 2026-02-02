# Subset a dic variable while preserving dic attributes

This function allows subsetting of dic variables while preserving their
dic attributes, as well as any haven labels and value labels.

## Usage

``` r
# S3 method for class 'dic'
x[i = TRUE, ...]

# S3 method for class '`dic<-`'
x[i, ..., value]
```

## Arguments

- x:

  An object of class dic.

- i:

  Elements to be selected.

- ...:

  further arguments passed to the subset function.

## Value

A dic variable
