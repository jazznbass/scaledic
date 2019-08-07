## ----setup, include = FALSE----------------------------------------------
library(knitr)
library(dplyr)
library(tibble)
library(psych)
library(scaledic)

knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
  
)

## ----echo = FALSE--------------------------------------------------------
out <- tribble(
  ~Parameter, ~Meaning, ~Example,
  "LABEL", "A short label", "itrf_1",
  "SCALE", "Abreviation of the scale", "irtf",
  "SUB_SCALE", "Abrevation of the sub scale", "int",
  "SUB_SCALE_2", "Abrevation of the secon order sub scale", "shy",
  "SCALE_LABEL", "Name of the scale", "Integrated Teacher Report Fromula",
  "SUB_SCALE_LABEL", "Name of the sub scale", "internalizing problems",
  "SUB_SCALE_2_LABEL", "Name of the second order sub scale", "shyness",
  "ITEM", "Full text of the item", "Vermeidet die Teilnahme an Diskussionen im Unterricht",
  "INDEX", "An index number", "1",
  "VALUES", "Valid response values in an R manner", "1:5 (for integers 1 to 5) 1,2,3 (for integers 1, 2, 3)",
  "VALUE_LABELS", "Labels for each response value", "0 = nicht; 1 = leicht; 2 = mäßig; 3 = stark",
  "MISSING", "Missing values", "-888, -999",
  "TYPE", "Data type (factor, integer, float, reel)", "integer",
  "WEIGHT", "Reversion of item and its weight", "1 (positive), -1 (reverse), 1.5 (positive, weights 1.5 times)",
  "SOURCE", "Reference", "Casale et al. (2017)",
  "NOTE", "Further notes", "Item has low descrimination"
)

kable(out, caption = "Columns of a dictionary file")


## ------------------------------------------------------------------------
dic_ITRF %>% slice(1:3)  %>% kable()

## ------------------------------------------------------------------------
# Here we use the example dataset "ITRF" and the example dic file "dic_ITRF"
dat <- apply_dic(ITRF, dic_ITRF)

## ------------------------------------------------------------------------
list_scales(dat, labels = TRUE) %>%
  kable()

## ------------------------------------------------------------------------
dat <- check_values(dat, replace = NA)

## ------------------------------------------------------------------------
dat <- replace_missing(dat)

## ----message=FALSE, warning=FALSE----------------------------------------
# Imputation for items of the subscale ITRF_Ext
dat <- impute_missing(dat, subscale = "Ext")

# Imputation for items of the subscale ITRF_Int
dat <- impute_missing(dat, subscale = "Int")


## ------------------------------------------------------------------------
dat %>% 
  select_scale(subscale = "Int") %>%
  psych::describe(fast = TRUE) %>%
  kable()

## ------------------------------------------------------------------------
dat %>% 
  select_scale(subscale = "Int") %>%
  names2item() %>%
  psych::describe(fast = TRUE) %>%
  kable()

## ------------------------------------------------------------------------
dat %>%
  select_scale(scale = "ITRF") %>%
  names2item(chars = 40, short = TRUE) %>%
  psych::fa(nfactors = 2) %>%
  print(sort = TRUE, cut = 0.3)

## ------------------------------------------------------------------------
dat$itrf_ext <- score_scale(dat, subscale = "Ext")
dat$itrf_int <- score_scale(dat, subscale = "Int")


## ------------------------------------------------------------------------
dat %>%
  select_scores() %>%
  psych::describe(fast = TRUE) %>%
  kable()
