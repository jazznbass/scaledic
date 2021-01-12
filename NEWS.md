
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

