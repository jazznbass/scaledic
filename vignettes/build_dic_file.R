## ----setup, include = FALSE---------------------------------------------------
library(knitr)
library(dplyr)
library(tibble)
library(psych)
library(scaledic)
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = ""
)

## ----echo=FALSE---------------------------------------------------------------
ex_scaledic_data |> kable(caption = "ex_scaledic_data")

## ----echo=FALSE---------------------------------------------------------------
dic_file <- ex_scaledic_dic[, 1:2]
dic_file |>kable(caption = "Dictionary file")

## -----------------------------------------------------------------------------
dat_dic <- apply_dic(ex_scaledic_data, dic_file)

## ----echo=FALSE---------------------------------------------------------------
dic_file <- ex_scaledic_dic |>select(1,2, type)
dic_file |>kable(caption = "Dictionary file")

## -----------------------------------------------------------------------------
dat_dic <- apply_dic(ex_scaledic_data, dic_file)

## ----echo=FALSE---------------------------------------------------------------
dic_file <- ex_scaledic_dic |>select(1,2, weight, type)
dic_file |>kable(caption = "Dictionary file")

## -----------------------------------------------------------------------------
dat_dic <- apply_dic(ex_scaledic_data, dic_file)

## ----echo=FALSE---------------------------------------------------------------
dic_file <- ex_scaledic_dic |>select(1,2, "values", "weight", "type")
dic_file |>kable(caption = "Dictionary file")

## -----------------------------------------------------------------------------
dat_dic <- apply_dic(ex_scaledic_data, dic_file)

## -----------------------------------------------------------------------------
dat_dic <- check_values(dat_dic, report = TRUE, replace = NA) 

## -----------------------------------------------------------------------------
dat_dic |> kable(caption = "data frame with replaced invalid values")

## ----echo=FALSE---------------------------------------------------------------
dic_file <- ex_scaledic_dic |>select(1,2, "values", "value_labels", "weight", "type")
dic_file |>kable(caption = "Dictionary file")

## -----------------------------------------------------------------------------
dat_dic <- apply_dic(ex_scaledic_data, dic_file) |>check_values(replace = NA)

## -----------------------------------------------------------------------------
dat_dic$rel_1

## -----------------------------------------------------------------------------
dat_dic$sui_1

## -----------------------------------------------------------------------------
dat_dic$gender

## -----------------------------------------------------------------------------
dat_dic$age

## ----echo=FALSE---------------------------------------------------------------
dic_file <- ex_scaledic_dic |>select(1,2, values, value_labels, weight, type, missing)
dic_file |>kable(caption = "Dictionary file")

## -----------------------------------------------------------------------------
dat_dic <- apply_dic(ex_scaledic_data, dic_file)

## -----------------------------------------------------------------------------
dat_dic <- apply_dic(ex_scaledic_data, dic_file, check_values = TRUE)

## -----------------------------------------------------------------------------
dat_dic %>%
  slice(9:20) |>
  kable(caption = "Extract from the data frame with replaced missing values and checked values")

## ----echo=FALSE---------------------------------------------------------------
dic_file <- ex_scaledic_dic
dic_file |>kable(caption = "Dictionary file")

## -----------------------------------------------------------------------------
dat_dic <- apply_dic(ex_scaledic_data, dic_file, check_values = TRUE)

## -----------------------------------------------------------------------------
dat_dic |>
  select_items(scale == "rel") |>
  descriptives()

## -----------------------------------------------------------------------------
dat_dic |>
  select_items(scale == "rel") |>
  rename_items() %>%
  descriptives() %>%
  kable()

