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


