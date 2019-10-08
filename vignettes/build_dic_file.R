## ----setup, include = FALSE----------------------------------------------
library(knitr)
library(dplyr)
library(tibble)
library(psych)
library(sjPlot)
library(scaledic)
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----echo = FALSE--------------------------------------------------------
out <- tribble(
  ~Parameter, ~Meaning, ~Example,
  "label", "A short item label", "itrf_1",
  "scale", "Abreviation of the scale the item belongs to", "irtf",
  "sub_scale", "Abrevation of the sub scale", "int",
  "sub_scale_2", "Abrevation of the second order sub scale", "AD",
  "scale_label", "Name of the scale", "Integrated Teacher Report Form",
  "sub_scale_label", "Name of the sub scale", "internalizing problems",
  "scale_2_label", "Name of the second order sub scale", "Anxious/Depressed",
  "item", "Full text of the item", "Complains of headaches or stomach aches",
  "index", "An index number", "1",
  "values", "Valid response values in an R manner", "1:5 (for integers 1 to 5) 1,2,3 (for integers 1, 2, 3)",
  "value_labels", "Labels for each response value", "0 = not problematic; 1 = slightly problematic; 2 = problematic; 3 = strongly problematic",
  "missing", "Missing values", "-888, -999",
  "type", "Data type (factor, integer, float, real, character)", "integer",
  "weight", "Reversion of item and its weight", "1 (positive), -1 (reverse), 1.5 (positive, weights 1.5 times)",
  "source", "Reference", "Volpe et al. (2019)",
  "note", "Further notes", "Item has low descrimination"
)
kable(out, caption = "Columns of a dictionary file")


## ----echo = FALSE--------------------------------------------------------
dic_ITRF %>% slice(1:3)  %>% kable()

