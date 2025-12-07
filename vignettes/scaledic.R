## ----setup, include = FALSE---------------------------------------------------
library(knitr)
library(dplyr)
library(tibble)
library(scaledic)
library(stringr)
library(wmisc)

knitr::opts_chunk$set(
  message = FALSE,
  warning = FALSE,
  collapse = TRUE,
  comment = "#>"
)

## ----dic_example, echo=FALSE--------------------------------------------------
dic_itrf |>  
  select(item_name, item_label, scale, scale_label, subscale, subscale_label, values, value_labels, missing, type) |> 
  slice(2:4) |> 
  wmisc::nice_table(cols_align = list(left = 1:10))

## ----tab_dic_param, echo = FALSE----------------------------------------------
out <- tribble(
  ~Parameter, ~Meaning, ~Example,
  "item_name", "A short item name", "itrf_1",
  "item_label", "Full text of the item", "Vermeidet die Teilnahme an Diskussionen im Unterricht",
  #"scale", "Abreviation of the scale the item belongs to", "irtf",
  #"scale_label", "Name of the scale", "Integrated Teacher Report Form",
  #"subscale", "Abrevation of the sub scale", "int",
  #"subscale_label", "Name of the sub scale", "internalizing problems",
  "values", "Valid response values in an R manner", "1:5 (for integers 1 to 5) 1,2,3 (for integers 1, 2, 3)",
  "value_labels", "Labels for each response value", "0 = nicht; 1 = leicht; 2 = mäßig; 3 = stark",
  "missing", "Missing values", "-888, -999",
  "type", "Data type (factor, integer, float, real)", "integer",
  "weight", "Reversion of item and its weight", "1 (positive), -1 (reverse), 1.5 (positive, weights 1.5 times)",
)
wmisc::nice_table(out, caption = "Basic columns of a dictionary file", cols_align = list(left = 2:3))


## ----apply_dic----------------------------------------------------------------
# Here we use the example dataset "dat_itrf" and the example dic file "dic_itrf"
dat <- apply_dic(dat_itrf, dic_itrf)

## -----------------------------------------------------------------------------
# list_scales(dat, paste0(c("scale", "subscale", "subscale_2"), "_label"))

## -----------------------------------------------------------------------------
list_scales(dat, paste0(c("scale", "subscale", "subscale_2"), "_label")) |> wmisc::nice_table(cols_align = list(left = 2:3))

## ----check_values-------------------------------------------------------------
dat <- check_values(dat, replace = NA)

## ----impute_missing, eval = FALSE---------------------------------------------
# # Imputation for items of the subscale Ext
# dat <- impute_missing(dat, subscale == "Ext")
# 
# # Imputation for items of the subscale Int
# dat <- impute_missing(dat, subscale == "Int")
# 

## ----impute_missing_eval, include=FALSE---------------------------------------
dat <- impute_missing(dat, subscale == "Ext")
dat <- impute_missing(dat, subscale == "Int")

## -----------------------------------------------------------------------------
# dat |> select_items(subscale == "Int")

## -----------------------------------------------------------------------------
dat |>  
  select_items(subscale == "Int") |> 
  wmisc::nice_descriptives(round = 1)

## -----------------------------------------------------------------------------
dat |> 
  select_items(subscale == "Int") |> 
  rename_items() |> 
  wmisc::nice_descriptives(round = 1)

## ----exploratory_fa-----------------------------------------------------------
dat |> 
  select_items(scale == "ITRF") |>
  rename_items(pattern = "({reverse}){subscale}_{subscale_2}: {label}", max_chars = 70) |> 
  psych::fa(nfactors = 4) |> 
  wmisc::nice_efa(cut = 0.4)

## ----item_analysis------------------------------------------------------------
scales <- ex_itrf |> get_scales(
  'Anxious/Depressed' = subscale_2 == "APD",
  'Oppositional/Disruptive' = subscale_2 == "OPP",
  "Socially Withdrawn" = subscale_2 == "SW",
  "Academic Productivity/Disorganization" = subscale_2 == "AD"
)
wmisc::nice_alpha_table(dat, scales = scales)
wmisc::nice_item_analysis(dat, scales = scales)

## ----lavaan_model-------------------------------------------------------------
model <- lavaan_model(scales, orthogonal = FALSE)

## ----cat_lavaan_model, comment = "", echo = FALSE-----------------------------
cat(model)

## ----lavaan_cfa, comment = ""-------------------------------------------------
fit <- lavaan::cfa(model = model, data = dat)
wmisc::nice_sem(fit)


## ----scores-------------------------------------------------------------------
dat$itrf_ext <- score_scale(dat, scale == "ITRF" & subscale == "Ext", label = "Externalizing")
dat$itrf_int <- score_scale(dat, scale == "ITRF" & subscale == "Int", label = "Internalizing")

## ----desc_scores--------------------------------------------------------------
dat[, c("itrf_ext", "itrf_int")] |> 
  rename_items() |> 
  wmisc::nice_descriptives(round = 1)

## -----------------------------------------------------------------------------
ex_normtable_int |> slice(1:10) |> wmisc::nice_table()

## -----------------------------------------------------------------------------
dat$raw_int <- score_scale(dat, subscale == "Int", sum = TRUE, max_na = 0)
dat$raw_ext <- score_scale(dat, subscale == "Ext", sum = TRUE, max_na = 0)

## -----------------------------------------------------------------------------
dat$T_int <- lookup_norms(dat$raw_int, normtable = ex_normtable_int, to = "T")
dat$T_ext <- lookup_norms(dat$raw_ext, normtable = ex_normtable_ext, to = "T")

## -----------------------------------------------------------------------------
dat$PR_int <- lookup_norms(dat$raw_int, normtable = ex_normtable_int, to = "PR")
dat$PR_ext <- lookup_norms(dat$raw_ext, normtable = ex_normtable_ext, to = "PR")

## -----------------------------------------------------------------------------

cols <- paste0(c("raw", "T", "PR"), rep(c("_int", "_ext"), each = 3))
wmisc::nice_table(
  dat[1:10, cols], 
  cols_label = list(raw_int = "Raw", T_int = "T", PR_int = "PR", raw_ext = "Raw", T_ext = "T", PR_ext = "PR"),
  label_na = "-",
  spanner = list(Internalizing = 1:3, Externalizing = 4:6)
)

