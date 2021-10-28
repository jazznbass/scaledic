## ----setup, include = FALSE---------------------------------------------------
library(knitr)
library(dplyr)
library(tibble)
library(psych)
library(scaledic)
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----tab_dic_param, echo = FALSE----------------------------------------------
out <- tribble(
  ~Parameter, ~Meaning, ~Example,
  "item_name", "A short item label", "itrf_1",
  "scale", "Abreviation of the scale the item belongs to", "irtf",
  "subscale", "Abrevation of the sub scale", "int",
  "subscale_2", "Abrevation of the second order sub scale", "AD",
  "scale_label", "Name of the scale", "Integrated Teacher Report Form",
  "subscale_label", "Name of the sub scale", "internalizing problems",
  "subscale_2_label", "Name of the second order sub scale", "Anxious/Depressed",
  "item_label", "Full text of the item", "Complains of headaches or stomach aches",
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


## -----------------------------------------------------------------------------
n <- 20
n_var <- 10
dat <- matrix(sample(1:5, n * n_var, replace = TRUE), ncol = n_var)
dat <- as.data.frame(dat)
names(dat) <- LETTERS[1:n_var]

dic <- data.frame(
  item_name = LETTERS[1:n_var],
  item_label = paste("Item", LETTERS[1:n_var]),
  scale = "letters",
  scale_label = "Series of letters",
  values = c("1:5", "1,5"),
  value_labels = "1 = low; 2; 3; 4; 5 = high",
  type = c("integer"),#, "integer", "integer", "float", "factor"),
  weight = 1,
  missing = "-999"
)

dat <- apply_dic(dat, dic)
dat <- check_values(dat)

dat$mean_letters <- score_scale(dat, scale == "letters", label = "Average letter value")



