# scaledic 0.2.x

## New functions

- `lookup_norms()`: Turns raw scores to normscores with the help of a normtable. Added to example normtables for the ex_itrf example.

```.r
normtable <- data.frame(
  group = c(6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8),
  raw = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
  T = c(40, 42, 44, 46, 48, 50, 52, 54, 56, 58, 60, 37, 39, 41, 43, 45, 47,
        49, 51, 53, 55, 57)
)

rawscores <- c(5,5,3,1)
group <- c("6", "8", "6", "8")
lookup_norms(rawscores, group, normtable)

```

- `get_scales()`: A wrapper around `select_items()` to extract multiple scale definitions:

```.r
scales <- get_scales(ex_itrf,
  'APD' = subscale_2 == "APD",
  'OPP' = subscale_2 == "OPP",
  "SW" = subscale_2 == "SW",
  "AD" = subscale_2 == "AD"
)

# is identical to:

scales <- list(
  'APD' = select_items(dat, subscale_2 == "APD", names_only = TRUE),
  'OPP' = select_items(dat, subscale_2 == "OPP", names_only = TRUE),
  "SW" = select_items(dat, subscale_2 == "SW", names_only = TRUE),
  "AD" = select_items(dat, subscale_2 == "AD", names_only = TRUE)
)
```

- `get_dic_attribute()`. Returns a list with dic attributes for a dataframe.

```.r
ex_itrf %>% 
  select_items(subscale == "Int") %>% 
  get_dic_attribute("item_label")

```

## Reworkek

- `rename_items()`: Much more versatile syntax applying the tidyvers glue function. Old functionality kept but throws a deprecated warning.)

``` .r
ex_itrf %>% select(1:5) %>%
  rename_item("{reverse}{item_name}: {item_label}")
```

# scaledic 0.1.27

Bugs:  
- `haven_dic()`: labels with length > 0 are not set (a warning is thrown).

Changes:
- `apply_dic()`: replaces linebreaks in value_labels with ";"

# scaledic 0.1.20

New functions:
- `set_label()`: Sets dic information. Will replace set_dic.

Changes:
- `apply_dic()`: new attribute `coerce_class`. When type of dic is numeric it checks if class of variable is numeric too. If not, it throws a message and coerces type to numeric. When `coerce_class = FALSE`, check is applied but no coercion.
- dictionary file: New variable "active". When a column "active" exists, all tows with active != 1 are dropped.
- `exploratory_fa()`: explained variance added to output

# scaledic 0.1.16 (2020-01-12)
- A lot of bug fixes


# scaledic 0.1.15 (2020-12-12)
- implemented vctrs vec_ptype2 vec_cast S3 methods for dic variables for better integration of scaledic into dyplyr


# scaledic 0.1.12 (2020-09-22)
- New function: `comnbine_data_frame()`. Will combine several dataframes (rows and columns) into a new data frame while keeping the dic information


# scaledic 0.1.11 (2020-09-22)
- `apply_dic()`: impute_values before scoring when `impute_values = TRUE`.
- `select_scale()` is deprecated. Instead use `select_items(data, filter)`
- `get_index()` is deprecated. Instead use `select_items(data, filter, names_only = TRUE)`.


# scaledic 0.1.10 (2020-06-26)
- Internally reworked the values and value_labels attributes.

# scaledic 0.1.9 (2020-04-16)
- New function: `score_from_dic()`. Now, a dictionary file can contain information for building scale score (`score_filter`, `score_function`, `item_label`, `item_name`. This function extracts these infromation from the dic file and returns a data frame with the new score variable(s).

# scaledic 0.1.8.2 (2020-04-07)

- New function: `rename_by_list()` for renaming variables in a data file based on a provided list with named strings.


# scaledic 0.1.8.0 (2020-03-25)

- New function: `descriptives()` for returning a table with descriptive statistics for variables in a data.frame.
- `apply_dic()`: If a charcter string is passed to the `dic` argument instead of a data frame, the function will take this as a filename and tries to load the file.
- `get_dic()` renamed to `backup_dic()`

# scaledic 0.1.7.10

- New function: `alpha_table()` for creating an item analyses for mutiple scales
- new function: `psych_fa()` for returning a loading matrix of a factor analyses (based on the psych package)

# scaledic 0.1.7.9

- New format for specifiying selection of items by dic information: now uses logical operations including all dic attributes. Examples: `select_scale(data, scale == "ITRF")`, `get_index(data, scale == "ITRF" & subscale = "Int" & weight == 1)`. The old specification is still working but deprecated. New format is applicable in: `get_index()`, `select_scale()`, `impute_missing()`, `score_scales()`.
- dicfile: `sub_scale` and `sub_scale_2` renamed to `subscale` and `subscale_2`. Old dic files still work as variables are renamed to new versions on import.
- Internal reorganization: dic attributes `sub_scale` and `sub_scale_2` renamed to `subscale` and `subscale_2`.

# scaledic 0.1.7.8

- `score_scale()`: New argument `sum`. If TRUE, calculated the sum and if FALSE, calculates the mean. Argument `label` can be used to set a dic label for the resulting score variable. When label is left NULL, a label will be generated automatically.
- `apply_dic()`: Agrument `set_label_attr` is now `TRUE` by default.

# scaledic 0.1.7.7

- Internal rename of dic attribute names.
- Variable name of *dic* file is now "name" by default. `label` is still possible but will be renamed internally.


# scaledic 0.1.7.6

- New class `dic` with a `[` method. This allows to subset dataframes with dictionaries without loosing the dic information. The class is added as the fist class to each variable in a dataframe that contains dic information.
- `print()` method for class *dic*.
- *dic_file*: new *type* values possible: 'numeric' or 'double'
- `get_index()` now takes any dictionary attribute and returns an error message when an invalid dic attribute is selected.
- `dic_haven()`: Adds Hmisc/haven labels from dic infos
- `haven_dic()`: Adds dic infos from haven labels
- `apply_dic()`: New argument `set_label_attr`. If TRUE executes `dic_haven()`.

# scaledic 0.1.7

- `apply_dic()`: Bug with factor type solved

# scaledic 0.1.6

- new function `build_scaledic_skeleton()`: Creates an Excel file with a template for a dic file.
- `list_scales()`: new argument `n_items` for displaying number of items per scale.
- `list_scales()`: new argument `char_na` for displaying NAs.
- *dic_file*: new *type* values possible: 'string', 'character', or 'characters'
- `names2labels()`: new argument `prefix` takes character vector with possible values 'scale', 'subscale', 'subsclae2', 'reverse', and 'weight' adding the respective information before the item.
- `names2label()`: deletet arguments `short` and `reverse`.
- `names2label()`: New arguments `char_weight`, `char_sep`, and `char_prefix_end` help to specify look of output.
- `apply_dic()`: new `replace_missing` argument replaces missing values as defined in the dic file with NAs.

