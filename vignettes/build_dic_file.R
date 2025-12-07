## ----setup, include = FALSE---------------------------------------------------
library(knitr)
library(dplyr)
library(tibble)
library(wmisc)
library(scaledic)
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = ""
)

## ----echo=FALSE---------------------------------------------------------------
ex_scaledic_data |> wmisc::nice_table(title = "ex_scaledic_data")

## ----echo=FALSE---------------------------------------------------------------
dic_file <- ex_scaledic_dic[, 1:2]
dic_file |> wmisc::nice_table(title = "Dictionary file", cols_align = list(left = 2))

## -----------------------------------------------------------------------------
dat_dic <- apply_dic(ex_scaledic_data, dic_file)

## ----echo=FALSE---------------------------------------------------------------
dic_file <- ex_scaledic_dic |>select(1,2, type)
dic_file |> wmisc::nice_table(title = "Dictionary file", cols_align = list(left = 2:3))

## -----------------------------------------------------------------------------
dat_dic <- apply_dic(ex_scaledic_data, dic_file)

## ----echo=FALSE---------------------------------------------------------------
dic_file <- ex_scaledic_dic |>select(1,2, weight, type)
dic_file |> wmisc::nice_table(title = "Dictionary file", cols_align = list(left = 2:4))

## -----------------------------------------------------------------------------
dat_dic <- apply_dic(ex_scaledic_data, dic_file)

## ----echo=FALSE---------------------------------------------------------------
dic_file <- ex_scaledic_dic |>select(1,2, "values", "weight", "type")
dic_file |> wmisc::nice_table(title = "Dictionary file", cols_align = list(left = 2:5))

## -----------------------------------------------------------------------------
dat_dic <- apply_dic(ex_scaledic_data, dic_file)

## -----------------------------------------------------------------------------
dat_dic <- check_values(dat_dic, replace = NA) 

## -----------------------------------------------------------------------------
dat_dic |> wmisc::nice_table(title = "data frame with replaced invalid values")

## ----echo=FALSE---------------------------------------------------------------
dic_file <- ex_scaledic_dic |>select(1,2, "values", "value_labels", "weight", "type")
dic_file |> wmisc::nice_table(caption = "Dictionary file", cols_align = list(left = "all"))

## -----------------------------------------------------------------------------
dat_dic <- ex_scaledic_data |> 
  apply_dic(dic_file) |>
  check_values(replace = NA)

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
dic_file |>wmisc::nice_table(title = "Dictionary file", cols_align = list(left = "all"))

## -----------------------------------------------------------------------------
dat_dic <- apply_dic(ex_scaledic_data, dic_file)

## ----echo=FALSE---------------------------------------------------------------
dic_file <- ex_scaledic_dic
dic_file |> wmisc::nice_table(title = "Dictionary file", cols_align = list(left = "all"))

## -----------------------------------------------------------------------------
dat_dic <- apply_dic(ex_scaledic_data, dic_file, check_values = TRUE)

## -----------------------------------------------------------------------------
dat_dic |>
  select_items(scale == "rel") |>
  wmisc::nice_descriptives(round = 2)

## -----------------------------------------------------------------------------
dat_dic |>
  select_items(scale == "rel") |>
  rename_items() |>
  wmisc::nice_descriptives(round = 2)

