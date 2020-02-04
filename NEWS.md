
# scaledic 0.1.7.6

- New class `dic` with a new `[` function. This allows to subset dataframes with dictionaries without loosing the dic information. The class is added as the fist class to each variable in a dataframe that contains dic information.
- *dic_file*: new *type* values possible: 'numeric' or 'double'
- *get_index()* now takes any dictionary attribute and returns an error message when an invalid dic attribute is selected.
- *dic_haven()*: Adds Hmisc/haven labels from dic infos
- *haven_dic()*: Adds dic infos from haven labels
- *apply_dic()*: New argument `set_label_attr`. If TRUE executes `dic_haven()`.

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

