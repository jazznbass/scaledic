# Combine data frames with dic information

Combine data frames (rows and columns) and keep dic information.

## Usage

``` r
combine_data_frames(...)
```

## Arguments

- ...:

  data frames with dic information to be combined.

## Value

A combined data frame with all rows and columns from the provided data
frames.

## Details

This function is useful when you want to join several data frames that
contain dic information into one data frame. When a variable that is
entailed in both data frames has dic information, the resulting data
frame will keep the dic information of the data frame that is first
listed as an argument. If a variable is present in one data frame but
not in another, the missing values will be filled with NA.
