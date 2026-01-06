# Safely parse a `values` specification from a dic file

Supported syntax:

- Integers: "1, 2, 3"

- Integer ranges: "5:11" (inclusive)

- Mixed: "1, 3:5, 9"

- Float: "min, max" (two numeric values)

- Character/factor: "'m', 'f', 'd'" or "\\m\\, \\f\\"

## Usage

``` r
.extract_values(values, type, item, field)
```

## Details

No code evaluation is performed.
