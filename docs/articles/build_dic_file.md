# How to build a dictionary file

In the following examples I will lay out the construction of a
dictionary file.

First, we need an example data.frame to work with (i.e. the
`ex_scaledic_data` example dataset):

[TABLE]

Second, we need a dictionary file that is also a **data.frame**. Each
row of a dictionary file addresses a variable (also called *item* in
this context) and each column holds a specific attribute. These
attributes can be divided in two classes. Firstly, *predefined*
attributes with a specific name and *additional* attributed that can
have any name. The *predefined* attributes are identified by their
specific names. Theses are: `item_name`, `item_label`, `values`,
`value_labels`, `missing`, `weight`, and `type`[^1].

*Additional* attributes can have any name and are mainly used for
selecting items or displaying additional information of an item (e.g.,
the scale and subscale an item belongs to; the reference and the author
of an item; the translation of an item).

## Building a dictionary step by step

We start the example with a very simple dictionary file containing only
item labels and the corresponding item names (i.e, the corresponding
variable names in our data frame)[^2]. The dictionary file has two
columns, `item_name` and `item_label`:

[TABLE]

We combine the *data* file and the *dic* file with the
[`apply_dic()`](../reference/apply_dic.md) function:

``` r
dat_dic <- apply_dic(ex_scaledic_data, dic_file)
! (dic)
1: Type is missing and is estimated as 'character'.
2: Type is missing and is estimated as 'double'. (8x)
3: Type is missing and is estimated as 'integer'. (3x)
```

We get the message that the dic file does not contain information of the
data*type* (e.g. `integer`, `factor`, `character`). The `type` attribute
is used in various *scaledic* functions. For example when the data is
checked for invalid values, missing values are imputed, or scales are
scored. A variable can be one of the following types: `integer` for
numbers without decimals, `float` for numbers with decimals, `character`
for variables with text, and `factor` for variables with text or numbers
that are levels of a factor. *Scaledic* will estimate the data type from
the given data when the `type` attribute is not provided in the dic
file.

Let us add `type` information to the dic file:

[TABLE]

Most item values are of type `integer` (the item has only whole numbers)
with the exception of `gender` which is of type `factor` (i.e. it has a
nominal scale with several levels).

``` r
dat_dic <- apply_dic(ex_scaledic_data, dic_file)
! (dic)
1: Type 'real' or 'float' is replaced by 'double'.
```

Now we can add `weight` information to the dic file. The `weight`
attribute tells various *scaledic* functions a) whether the item is
inverted or not, and b) how the values of an item are weighted. If a
weight value is unsigned (e.g. `1`), the item is not inverted. If a
weight value has a negative sign (e.g. `-1`), the item is inverted. If
the (absolute) value is not `1`, the item will be weighted when
calculating the item scores (e.g. `1.5` will give an item a weight of
1.5).

[TABLE]

In this example, all `weights` are `1` and no item is inverted.

Again, we join dic and data file:

``` r
dat_dic <- apply_dic(ex_scaledic_data, dic_file)
! (dic)
1: Type 'real' or 'float' is replaced by 'double'.
```

Now we add the `values` attribute to the dic file. `values` defines the
valid values that a variable can take. Explicitly defining the `values`
makes it possible to automatically identify invalid values in a data
frame (e.g. typos). `values` are also necessary to reverse score an
inverted item. For coding the `values`, you provide the possible values
separated with a comma (e.g., `1, 2, 3, 4` for the integers 1 to 4). It
is also possible to use a colon to define a range of integers (e.g.,
`5:11` indicated all integers starting with 5 and ending with 11). For
type `float`, values represent the maximum and the minimum of valid
values (e.g. `5, 11` for all values within 5 and 11 including decimal
numbers). For variables of type `factor` or `character` you may want to
provide text values. in that case, put these values within quotes (e.g.,
`'m', 'f', 'd'` indicates three valid text entries).

Here is the dic file with added `values`:

[TABLE]

``` r
dat_dic <- apply_dic(ex_scaledic_data, dic_file)
! (dic)
1: Type 'real' or 'float' is replaced by 'double'.
```

Invalid values can be replaced automatically with the
[`check_values()`](../reference/check_values.md) function.

``` r
dat_dic <- check_values(dat_dic, replace = NA) 
! (check_values)
1: Replaced the following invalid values with NA:
  'rel_2' is 66 at row 3
  'rel_3' is -999 at row 14
  'rel_4' is 11, -999 at rows 9, 18
  'rel_5' is 66 at row 11
  'sui_1' is 55 at row 10
  'sui_2' is -999 at row 9
  'sui_4' is 66 at row 17
  'age' is 13, -999, 13 at rows 4, 12, 16
```

``` r
dat_dic |> wmisc::nice_table(title = "data frame with replaced invalid values")
```

[TABLE]

Next, we add `value_labels`. Value labels give longer labels for all or
some of the values. Value labels are coded in the form `value = label`
with a semicolon separating each entry.
`m = male; f = female; d = diverse` codes three value label (quotes are
not necessary).

Here is the dic file with added `value_labels`:

| item_name | item_label | values | value_labels | weight | type |
|----|----|----|----|----|----|
| rel_1 | How often do you attend church or other religious meetings? | 1:6 | 1 = Never; 2 = Once a year or less; 3 = A few times a year; 4 = A few times a month; 5 = Once a week; 6 = More than once/week | 1 | integer |
| rel_2 | How often do you spend time in private religious activities, such as prayer, meditation or Bible study? | 1:6 | 1 = Rarely or never; 2 = A few times a month; 3 = Once a week; 4 = Two or more times/week; 5 = Daily; 6 = More than once a day | 1 | integer |
| rel_3 | In my life, I experience the presence of the Divine (i.e., God) | 1:5 | 1 = Definitely not true; 2 = Tends not to be true; 3 = Unsure; 4 = Tends to be true; 5 = Definitely true of me | 1 | integer |
| rel_4 | My religious beliefs are what really lie behind my whole approach to life | 1:5 | 1 = Definitely not true; 2 = Tends not to be true; 3 = Unsure; 4 = Tends to be true; 5 = Definitely true of me | 1 | integer |
| rel_5 | I try hard to carry my religion over into all other dealings in life | 1:5 | 1 = Definitely not true; 2 = Tends not to be true; 3 = Unsure; 4 = Tends to be true; 5 = Definitely true of me | 1 | integer |
| sui_1 | Did you feel tense in the last week? | 0:4 | 0 = not at all; 4 = extremely | 1 | integer |
| sui_2 | Did you feel blue in the last week? | 0:4 | 0 = not at all; 4 = extremely | 1 | integer |
| sui_3 | Did you feel irritated in the last week? | 0:4 | 0 = not at all; 4 = extremely | 1 | integer |
| sui_4 | Did you feel inferior in the last week? | 0:4 | 0 = not at all; 4 = extremely | 1 | integer |
| sui_5 | Did you have problems falling asleep in the last week? | 0:4 | 0 = not at all; 4 = extremely | 1 | integer |
| gender | gender | 'm', 'f', 'd' | m = male; f = female; d = diverse | 1 | factor |
| age | age | 5, 11 | 5 = min; 11 = max | 1 | float |

``` r
dat_dic <- ex_scaledic_data |> 
  apply_dic(dic_file) |>
  check_values(replace = NA)
! (check_values)
1: Replaced the following invalid values with NA:
  'rel_2' is 66 at row 3
  'rel_3' is -999 at row 14
  'rel_4' is 11, -999 at rows 9, 18
  'rel_5' is 66 at row 11
  'sui_1' is 55 at row 10
  'sui_2' is -999 at row 9
  'sui_4' is 66 at row 17
  'age' is 13, -999, 13 at rows 4, 12, 16
! (dic)
1: Type 'real' or 'float' is replaced by 'double'.
```

Now lets see the coding for some of the variables:

``` r
dat_dic$rel_1
║How often do you attend church or other religious meetings? 
║Data type is integer
║Valid values: 1:6
║Value labels:
║  1 = Never
║  2 = Once a year or less
║  3 = A few times a year
║  4 = A few times a month
║  5 = Once a week
║  6 = More than once/week
║ 
║Length is 20 (0 NA; 0 invalid)
║ [1] 1 3 1 2 5 3 3 4 2 6 6 6 3 6 2 3 4 5 6 2
```

Valid values are all integers from 1 to 6 and each value has a label.

``` r
dat_dic$sui_1
║Did you feel tense in the last week? 
║Data type is integer
║Valid values: 0:4
║Value labels:
║  0 = not at all
║  4 = extremely
║ 
║Length is 20 (1 NA; 0 invalid)
║ [1]  2  4  3  0  1  3  0  0  1 NA  4  1  2  4  2  1  3  1  0  1
```

Here, valid values are `0, 1, 2, 3, 4` (short `0:4`) but only the poles
(`0` and `4`) have labels.

``` r
dat_dic$gender
║gender 
║Data type is factor
║Value labels:
║  m = male
║  f = female
║  d = diverse
║ 
║Length is 20 (1 NA; 0 invalid)
║ [1] f    d    d    f    f    d    f    f    f    f    m    m    m    m    d   
║[16] d    <NA> m    m    m   
║Levels: m f d
```

`gender` is of type factor. The valid values `'m', 'f', 'd'` have been
turned into three factor levels with the corresponding labels.

> Caution: The labels provided in the `value_labels` attribute are not
> automtically turned into the levels of a factor. This is because R
> internally would turn the values (here `'m', 'f', 'd'`) into integers
> (here `1, 2, 3`)

``` r
dat_dic$age
║age 
║Data type is double
║Valid values: From 5 to 11
║Value labels:
║  5 = min
║  11 = max
║ 
║Length is 20 (3 NA; 0 invalid)
║ [1] 10.5  6.5 10.5   NA  8.0  6.0  6.0 10.0  8.5  8.5  7.5   NA  7.0  7.5  8.5
║[16]   NA 10.5 10.0  8.0  6.0
```

`age` can take all numbers from 5 to 11 (minimum is 5 and maximum is
11).

It is a common practice to code missing values with a specific number
(rather than just leaving an empty entry on a datasheet). In the example
dataset used here, `-999` is used as a missing value. To account for
this, we add the `missing` attribute to the *dic* file containing the
missing number (e.g., `-999`). Multiple missing values are separated by
commas (e.g. `-99, -77`).

[TABLE]

The values within the `missing` attributes are automatically replaced
with NA when joining a data with a dic file:

``` r
dat_dic <- apply_dic(ex_scaledic_data, dic_file)
! (dic)
1: Type 'real' or 'float' is replaced by 'double'.
! (replace_missing)
1: Replaced 1 missing value in 'age' with NA
2: Replaced 1 missing value in 'rel_3' with NA
3: Replaced 1 missing value in 'rel_4' with NA
4: Replaced 1 missing value in 'sui_2' with NA
```

To turn off this behavior, set the argument `replace_missing = FALSE`.

In the last step of this tutorial we will add information about the
scales that the items belong to. Therefore, we add a new attribute
called `scale` to the dic file and another attribute `scale_label` for a
longer description (we could have named these attributes in any other
way as they are not *predefined* attributes):

[TABLE]

``` r
dat_dic <- apply_dic(ex_scaledic_data, dic_file, check_values = TRUE)
! (check_values)
1: Replaced the following invalid values with NA:
  'rel_2' is 66 at row 3
  'rel_4' is 11 at row 9
  'rel_5' is 66 at row 11
  'sui_1' is 55 at row 10
  'sui_4' is 66 at row 17
  'age' is 13, 13 at rows 4, 16
! (dic)
1: Type 'real' or 'float' is replaced by 'double'.
! (replace_missing)
1: Replaced 1 missing value in 'age' with NA
2: Replaced 1 missing value in 'rel_3' with NA
3: Replaced 1 missing value in 'rel_4' with NA
4: Replaced 1 missing value in 'sui_2' with NA
```

You can use the scale attribute to select items:

``` r
dat_dic |>
  select_items(scale == "rel") |>
  wmisc::nice_descriptives(round = 2)
```

[TABLE]

and the [`rename_items()`](../reference/rename_items.md) function to get
the item labels:

``` r
dat_dic |>
  select_items(scale == "rel") |>
  rename_items() |>
  wmisc::nice_descriptives(round = 2)
```

[TABLE]

[^1]: Actually there are three more predefined attributes `class`,
    `scale_function`, and `scale_filter` that are not explained in this
    tutorial.

[^2]: The easiest way to create a dictionary file is to use a
    spreadsheet program and to import the file.
