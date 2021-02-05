## ----setup, include = FALSE---------------------------------------------------
library(knitr)
library(dplyr)
library(tibble)
#library(MASS)
library(scaledic)

knitr::opts_chunk$set(
  message = FALSE,
  warning = FALSE,
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE---------------------------------------------------------------
#  newdat <- select(dat, "iq", "age")
#  newdat <- describe(newdat)
#  newdat <- round(newdat, 2)
#  kable(newdat)

## ----eval=FALSE---------------------------------------------------------------
#  kable(round(describe(select(dat, "iq", "age")), 2))

## ----eval=FALSE---------------------------------------------------------------
#  dat %>%
#    select("iq", "age") %>%
#    describe() %>%
#    round(2) %>%
#    kable()

## ----echo = FALSE-------------------------------------------------------------
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
  "note", "Further notes", "Item has low descrimination"
)
kable(out, caption = "Columns of a dictionary file")


## ----dic_example--------------------------------------------------------------
dic_ITRF %>%
  slice(1:3) %>%
  kable()

## ----apply_dic----------------------------------------------------------------
# Here we use the example dataset "ITRF" and the example dic file "dic_ITRF"
dat <- apply_dic(ex_itrf, dic_ITRF)

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

## ----include=FALSE------------------------------------------------------------
dat <- impute_missing(dat, subscale == "Ext")
dat <- impute_missing(dat, subscale == "Int")

## ----descriptives-------------------------------------------------------------
dat %>%
  select_items(subscale == "Int") %>%
  descriptives(round = 1) %>%
  kable()

## -----------------------------------------------------------------------------
dat %>%
  select_items(subscale == "Int") %>%
  descriptives(round = 1, label = TRUE) %>%
  kable()

## -----------------------------------------------------------------------------
dat %>%
  select_items(scale == "ITRF") %>%
  rename_item(pattern = c("reverse", "subscale", "subscale_2", "label"), chars = 70) %>%
  exploratory_fa(nfactors = 4, cut = 0.4) %>%
  kable()

## -----------------------------------------------------------------------------
scales <- list(
  'APD' = select_items(dat, subscale_2 == "APD", names_only = TRUE),
  'OPP' = select_items(dat, subscale_2 == "OPP", names_only = TRUE),
  "SW" = select_items(dat, subscale_2 == "SW", names_only = TRUE),
  "AD" = select_items(dat, subscale_2 == "AD", names_only = TRUE)
)
alpha_table(dat, scales = scales) %>%
  kable()


## -----------------------------------------------------------------------------
model <- lavaan_model(scales, orthogonal = FALSE)

## ----comment = "", echo = FALSE-----------------------------------------------
cat(model)

## ----comment = ""-------------------------------------------------------------
fit <- lavaan::cfa(model = model, data = dat)
lavaan::summary(fit, fit.measures = TRUE)


## -----------------------------------------------------------------------------
dat$itrf_ext <- score_scale(dat, scale == "ITRF" & subscale == "Ext", label = "Externalizing")
dat$itrf_int <- score_scale(dat, scale == "ITRF" & subscale == "Int", label = "Internalizing")

## -----------------------------------------------------------------------------
dat %>%
  select_scores() %>%
  descriptives(round = 1, label = TRUE) %>%
  kable()

