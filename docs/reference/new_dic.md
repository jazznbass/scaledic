# Dictionary class low level constructor

This is a low-level constructor for dic objects. It is recommended to
use [`set_dic()`](set_dic.md) for adding dic attributes to variables or
data frames.

## Usage

``` r
new_dic(
  x,
  item_name = NULL,
  item_label = NULL,
  values = NULL,
  value_labels = NULL,
  missing = NULL,
  weight = 1,
  type = NULL,
  recodes = NULL,
  class = "item",
  ...,
  .list = NULL,
  .coerce_class = TRUE
)
```

## Arguments

- x:

  A variable

- item_name:

  Character

- item_label:

  Character

- values:

  Numeric or character vector with values. The vector can be named

- value_labels:

  Character of the form `value = label; value2 = label2`

- missing:

  Numeric or character vector with values

- weight:

  numeric

- type:

  defaults to data type of x

- recodes:

  Recoding information e.g. `4 = 1, .default = 0`

- class:

  default is "item"

- ...:

  further dic arguments (e.g. `source = "James (1891)"`)

- .list:

  An alternative way to provide class arguments as a list (overwrites
  previous arguments)

- .coerce_class:

  Logical. If TRUE, tries to coerce classes of 'x' if class does not
  match the `type` argument

## Value

An item of class dic.

## Details

Standard attributes are: `"item_name"`, `"item_label"`, `"weight"`,
`"type"`, `"values"`, `"value_labels"`, `"missing"`, `"recodes"`.

## See also

[`set_dic()`](set_dic.md)

## Examples

``` r
x <- new_dic(1:100, item_label = "The label of this item")
x
#> ║The label of this item 
#> ║Data type is integer
#> 
#> ║Length: 100 (0 NA; 0 invalid)
#> ║  [1]   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18
#> ║ [19]  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35  36
#> ║ [37]  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54
#> ║ [55]  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72
#> ║ [73]  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90
#> ║ [91]  91  92  93  94  95  96  97  98  99 100
```
