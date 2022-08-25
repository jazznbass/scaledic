## ----setup, include = FALSE---------------------------------------------------
library(knitr)
library(dplyr)
library(tibble)
library(scaledic)

knitr::opts_chunk$set(
  message = FALSE,
  warning = FALSE,
  collapse = TRUE,
  comment = "#>"
)

## ----tab_dic_param, echo = FALSE----------------------------------------------
out <- tribble(
  ~Parameter, ~Meaning, ~Example,
  "item_name", "A short item name", "itrf_1",
  "scale", "Abreviation of the scale the item belongs to", "irtf",
  "subscale", "Abrevation of the sub scale", "int",
  "subscale_2", "Abrevation of the second order sub scale", "shy",
  "scale_label", "Name of the scale", "Integrated Teacher Report Form",
  "subscale_label", "Name of the sub scale", "internalizing problems",
  "subscale_2_label", "Name of the second order sub scale", "shyness",
  "item_label", "Full text of the item", "Vermeidet die Teilnahme an Diskussionen im Unterricht",
  "index", "An index number", "1",
  "values", "Valid response values in an R manner", "1:5 (for integers 1 to 5) 1,2,3 (for integers 1, 2, 3)",
  "value_labels", "Labels for each response value", "0 = nicht; 1 = leicht; 2 = mäßig; 3 = stark",
  "missing", "Missing values", "-888, -999",
  "type", "Data type (factor, integer, float, real)", "integer",
  "weight", "Reversion of item and its weight", "1 (positive), -1 (reverse), 1.5 (positive, weights 1.5 times)",
  "source", "Reference", "Casale et al. (2017)",
  "note", "Further notes", "Item has low discrimination"
)
kable(out, caption = "Columns of a dictionary file")


## ----dic_example--------------------------------------------------------------
dic_itrf %>% 
  slice(2:4) %>%
  kable()

## ----apply_dic----------------------------------------------------------------
# Here we use the example dataset "dat_itrf" and the example dic file "dic_itrf"
dat <- apply_dic(dat_itrf, dic_itrf)

## ----list_scales--------------------------------------------------------------
list_scales(dat, labels = TRUE) %>%
  kable()

## ----check_values-------------------------------------------------------------
dat <- check_values(dat, replace = NA)

## ----impute_missing, eval = FALSE---------------------------------------------
#  # Imputation for items of the subscale Ext
#  dat <- impute_missing(dat, subscale == "Ext")
#  
#  # Imputation for items of the subscale Int
#  dat <- impute_missing(dat, subscale == "Int")
#  

## ----impute_missing_eval, include=FALSE---------------------------------------
dat <- impute_missing(dat, subscale == "Ext")
dat <- impute_missing(dat, subscale == "Int")

## ----descriptives-------------------------------------------------------------
dat %>% 
  select_items(subscale == "Int") %>%
  descriptives(round = 1) %>%
  kable()

## ----desc_labels--------------------------------------------------------------
dat %>% 
  select_items(subscale == "Int") %>%
  descriptives(round = 1, label = TRUE) %>%
  kable()

## ----exploratory_fa-----------------------------------------------------------
dat %>%
  select_items(scale == "ITRF") %>%
  rename_items(pattern = "({reverse}){subscale}_{subscale_2}: {label}", max_chars = 70) %>%
  exploratory_fa(nfactors = 4, cut = 0.4) %>%
  kable()

## ----item_analysis------------------------------------------------------------
scales <- list(
  'APD' = select_items(dat, subscale_2 == "APD", names_only = TRUE),
  'OPP' = select_items(dat, subscale_2 == "OPP", names_only = TRUE),
  "SW" = select_items(dat, subscale_2 == "SW", names_only = TRUE),
  "AD" = select_items(dat, subscale_2 == "AD", names_only = TRUE)
)
alpha_table(dat, scales = scales) %>%
  kable()


## ----lavaan_model-------------------------------------------------------------
model <- lavaan_model(scales, orthogonal = FALSE)

## ----cat_lavaan_model, comment = "", echo = FALSE-----------------------------
cat(model)

## ----lavaan_cfa, comment = ""-------------------------------------------------
fit <- lavaan::cfa(model = model, data = dat)
lavaan::summary(fit, fit.measures = TRUE)


## ----scores-------------------------------------------------------------------
dat$itrf_ext <- score_scale(dat, scale == "ITRF" & subscale == "Ext", label = "Externalizing")
dat$itrf_int <- score_scale(dat, scale == "ITRF" & subscale == "Int", label = "Internalizing")

## ----desc_scores--------------------------------------------------------------
dat %>%
  select_scores() %>%
  descriptives(round = 1, label = TRUE) %>%
  kable()

