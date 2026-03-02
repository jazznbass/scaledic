# Introduction to scaledic

## What is a dictionary file?

When you conduct research based on questionnaires or psychometric tests
(and you are working in R), you typically create a *data.frame* with one
column (variable) for each item on that questionnaire and one row for
each person who participated. You can only store a limited amount of
additional information about each item in that questionnaire within a
data.frame (or tibble). You can give a variable a name and define a
variable as a factor with appropriate levels. But basically, that is it.
You cannot, at least not conveniently, include a longer label for each
item, the name of a scale to which that item belongs to, information
about reverse coding, etc.

I call the collection of this additional information about items an
*item dictionary*. A dictionary contains a short label, a longer
description, scale affiliation, and more for each item.

## A dictionary file

A dictionary file is a table with one row for each variable and one
column for each attribute of those variables. The most convenient way to
create a dictionary file is in a spreadsheet program for later use with
data sets.

Here is an extract from an example *dic-file*:

|  | item_name | item_label | scale | scale_label | subscale | subscale_label | values | value_labels | missing | type |
|----|----|----|----|----|----|----|----|----|----|----|
| 2 | itrf_I_1 | Verbringt zu viel Zeit alleine | ITRF | Integrated teacher report form | Int | Internalizing | 0:3 | 0 = not problematic; 1 = slightly problematic; 2 = problematic; 3 = strongly problematic | -99 | integer |
| 3 | itrf_I_2 | Beschwert sich über Krankheit oder Schmerzen | ITRF | Integrated teacher report form | Int | Internalizing | 0:3 | 0 = not problematic; 1 = slightly problematic; 2 = problematic; 3 = strongly problematic | -99 | integer |
| 4 | itrf_I_4 | Vermeidet soziale Interaktionen | ITRF | Integrated teacher report form | Int | Internalizing | 0:3 | 0 = not problematic; 1 = slightly problematic; 2 = problematic; 3 = strongly problematic | -99 | integer |

A dictionary file can contain any additional attributes. This means that
you can add a column with any name to store relevant information
(e.g. the scale and scale label to which an item belongs, a translation
of the item name). However, there are some predefined attributes with a
specific meaning. The table below shows these attributes:

| Parameter | Meaning | Example |
|----|----|----|
| item_name | A short item name | itrf_1 |
| item_label | Full text of the item | Vermeidet die Teilnahme an Diskussionen im Unterricht |
| values | Valid response values in an R manner | 1:5 (for integers 1 to 5) 1,2,3 (for integers 1, 2, 3) |
| value_labels | Labels for each response value | 0 = nicht; 1 = leicht; 2 = mäßig; 3 = stark |
| missing | Missing values | -888, -999 |
| type | Data type (factor, integer, float, real) | integer |
| weight | Reversion of item and its weight | 1 (positive), -1 (reverse), 1.5 (positive, weights 1.5 times) |

## Apply a dictionary file

When you combine a dataset with a dictionary file, each variable in the
dataset that corresponds to a variable described in the dictionary is
completed with the given dictionary information.  
The resulting dataset is now ready for use with all other `scaledic`
functions.

The `apply_dic` function takes the name of the dataset and the
dictionary file and combines them. Missing values are replaced by NAs:

``` r
# Here we use the example dataset "dat_itrf" and the example dic file "dic_itrf"
dat <- apply_dic(dat_itrf, dic_itrf)
```

Let us take a look at all the scales in the dataset:

``` r
list_scales(dat, "subscale_label", "subscale_2_label")
```

| subscale_label | subscale_2_label                      |
|----------------|---------------------------------------|
| Externalizing  | Oppositional/Disruptive               |
| Externalizing  | Academic Productivity/Disorganization |
| Internalizing  | Socially Withdrawn                    |
| Internalizing  | Anxious/Depressed                     |

## Clean raw data

Firstly, we check for invalid values in the dataset (e.g., typos) and
replace them with NA:

``` r
dat <- check_values(dat, replace = NA)
```

Now we impute missing values:

``` r
# Imputation for items of the subscale Ext
dat <- impute_missing(dat, subscale == "Ext")

# Imputation for items of the subscale Int
dat <- impute_missing(dat, subscale == "Int")
```

## Select scales for analyszing

Let us look at the descriptive statistics for the *internalising*
subscale:

``` r
dat |> select_items(subscale == "Int")
```

[TABLE]

## See items instead of labels

It is more convenient to see the original items rather than the short
labels:

``` r
dat |> 
  select_items(subscale == "Int") |> 
  rename_items() |> 
  wmisc::nice_descriptives(round = 1)
```

[TABLE]

And then we analyse the factor structure. Here we use the
`rename_item()` function to get a more convenient description.

``` r
dat |> 
  select_items(scale == "ITRF") |>
  rename_items(pattern = "({reverse}){subscale}_{subscale_2}: {label}", max_chars = 70) |> 
  psych::fa(nfactors = 4) |> 
  wmisc::nice_efa(cut = 0.4)
```

[TABLE]

and provide item analyses

``` r
scales <- dat |> get_scales(
  'Anxious/Depressed' = subscale_2 == "APD",
  'Oppositional/Disruptive' = subscale_2 == "OPP",
  "Socially Withdrawn" = subscale_2 == "SW",
  "Academic Productivity/Disorganization" = subscale_2 == "AD"
)
wmisc::nice_alpha_table(dat, scales = scales)
```

[TABLE]

``` r
wmisc::nice_item_analysis(dat, scales = scales)
```

[TABLE]

and even a confirmatory factor analysis with the use of the lavaan
package.

``` r
model <- lavaan_model(scales, orthogonal = FALSE)
```

    Anxious_Depressed =~ itrf_E_1 + itrf_E_2 + itrf_E_3 + itrf_E_4 + itrf_E_5 + itrf_E_6 + itrf_E_14 + itrf_E_15
    Oppositional_Disruptive =~ itrf_I_20 + itrf_E_7 + itrf_E_8 + itrf_E_9 + itrf_E_10 + itrf_E_11 + itrf_E_12 + itrf_E_13 + itrf_E_16
    Socially_Withdrawn =~ itrf_I_1 + itrf_I_4 + itrf_I_5 + itrf_I_6 + itrf_I_13 + itrf_I_14 + itrf_I_16 + itrf_I_24
    Academic_Productivity_Disorganization =~ itrf_I_2 + itrf_I_7 + itrf_I_8 + itrf_I_9 + itrf_I_10 + itrf_I_11 + itrf_I_12 + itrf_I_15 + itrf_I_17 + itrf_I_19 + itrf_I_23

    Anxious_Depressed ~~ Oppositional_Disruptive + Socially_Withdrawn + Academic_Productivity_Disorganization
    Oppositional_Disruptive ~~ Socially_Withdrawn + Academic_Productivity_Disorganization
    Socially_Withdrawn ~~ Academic_Productivity_Disorganization

``` r
fit <- lavaan::cfa(model = model, data = dat)
wmisc::nice_sem(fit)
```

[TABLE]

## Build scale scores

Now we will create scores for the internalizing and externalizing
scales.

``` r
dat$itrf_ext <- score_scale(dat, scale == "ITRF" & subscale == "Ext", label = "Externalizing")
dat$itrf_int <- score_scale(dat, scale == "ITRF" & subscale == "Int", label = "Internalizing")
```

and get descriptives for those scores

``` r
dat |> 
  subset(select = c(itrf_ext, itrf_int)) |> 
  rename_items() |> 
  wmisc::nice_descriptives(round = 1)
```

[TABLE]

## Look up norms from a norm table

Many scales come with norm tables to convert raw scores to t-scores,
percentile ranks, etc.

The `lookup_norms` function helps with this conversion.

Firstly, you need a data frame (or Excel table etc) which includes
raw-scores and corresponding norm-scores.

Here is an example of such a table:

``` r
ex_normtable_int |> head() |> wmisc::nice_table()
```

| group | raw | T   | PR  | T_from_PR |
|-------|-----|-----|-----|-----------|
| all   | 0   | 42  | 26  | 43        |
| all   | 1   | 43  | 35  | 46        |
| all   | 2   | 44  | 42  | 48        |
| all   | 3   | 46  | 49  | 50        |
| all   | 4   | 47  | 55  | 51        |
| all   | 5   | 48  | 60  | 52        |

Then we need raw-scores from a scale. If they do not exist, you may use
the `score_scales` function to add sum scores. Therefore set the sum
argument to `TRUE`. By setting `max_na = 0`, we do not allow missing
values in any scale item:

``` r
dat$raw_int <- score_scale(dat, subscale == "Int", sum = TRUE, max_na = 0)
dat$raw_ext <- score_scale(dat, subscale == "Ext", sum = TRUE, max_na = 0)
```

Looks up T values:

``` r
dat$T_int <- lookup_norms(dat$raw_int, normtable = ex_normtable_int, to = "T")
dat$T_ext <- lookup_norms(dat$raw_ext, normtable = ex_normtable_ext, to = "T")
```

Or percentile ranks:

``` r
dat$PR_int <- lookup_norms(dat$raw_int, normtable = ex_normtable_int, to = "PR")
dat$PR_ext <- lookup_norms(dat$raw_ext, normtable = ex_normtable_ext, to = "PR")
```

Which can look like that:

[TABLE]
