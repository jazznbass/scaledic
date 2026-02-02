# Concatenate dic vectors while keeping dic metadata from the first argument

Keeps dic attributes from the first dic argument and restores them after
concatenation.

## Usage

``` r
# S3 method for class 'dic'
c(..., recursive = FALSE)
```

## Arguments

- ...:

  dic vectors to be concatenated.

- recursive:

  logical. Should the result be recursive if any of the inputs are
  lists? Default is FALSE.

## Value

A dic vector resulting from concatenation of the provided dic vectors,
with dic attributes taken from the first argument.

## Details

If multiple dic arguments are provided, dic attributes from the other
arguments are ignored.
