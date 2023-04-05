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
  "item_label", "Full text of the item", "Complains of headaches or stomach aches",
  "scale_label", "Name of the scale", "Integrated Teacher Report Form",
  "subscale_label", "Name of the sub scale", "internalizing problems",
  "subscale_2_label", "Name of the second order sub scale", "Anxious/Depressed",
  "values", "Valid response values in an R manner", "1:5 (for integers 1 to 5) 1,2,3 (for integers 1, 2, 3)",
  "value_labels", "Labels for each response value", "0 = not problematic; 1 = slightly problematic; 2 = problematic; 3 = strongly problematic",
  "missing", "Missing values", "-888, -999",
  "type", "Data type (factor, integer, float, real, character)", "integer",
  "weight", "Reversion of item and its weight", "1 (positive), -1 (reverse), 1.5 (positive, weights 1.5 times)",
  "source", "Reference", "Volpe et al. (2019)",
  "note", "Further notes", "Item has low descrimination"
)
kable(out, caption = "Columns of a dictionary file")


## ----echo = FALSE-------------------------------------------------------------
set.seed(21)
n <- 20
n_scale <- 5
scales <- c("rel", "sui")
n_var <- n_scale * length(scales)
dat <- cbind(
  matrix(sample(1:6, n_scale * n, replace = TRUE), ncol = n_scale),
  matrix(sample(0:4, n_scale * n, replace = TRUE), ncol = n_scale)
)
dat <- as.data.frame(dat)
names(dat) <- paste(rep(scales, each = n_scale), rep(1:n_scale, length(scales)), sep = "_")
dat$gender <- sample(c("m", "f", "d"), n, replace = TRUE)
dat$age <- sample(6:10, n, replace = TRUE)

## ----echo=FALSE---------------------------------------------------------------
dat %>% slice(1:10) %>% kable(caption = "First ten rows of the data frame")#rmarkdown::paged_table()

## ----echo=FALSE---------------------------------------------------------------
item_names <- c("How often do you attend church or other religious meetings?",
"How often do you spend time in private religious activities, such as prayer, meditation or Bible study?", "In my life, I experience the presence of the Divine (i.e., God)", "My religious beliefs are what really lie behind my whole approach to life", "I try hard to carry my religion over into all other dealings in life",
paste("Did you feel", c("tense", "blue", "irritated", "inferior"), "in the last week?"), "Did you have problems falling asleep in the last week?")

value_labels <- c("1 = Never; 2 = Once a year or less; 3 = A few times a year; 4 = A few times a month; 5 = Once a week; 6 = More than once/week", "1 = Rarely or never; 2 = A few times a month; 3 = Once a week; 4 = Two or more times/week; 5 = Daily; 6 = More than once a day", rep("1 = Definitely not true; 2 = Tends not to be true; 3 = Unsure; 4 = Tends to be true; 5 = Definitely true of me", 3),
  rep(c("0 = not at all; 1; 2; 3; 4 = extremely"), 5)
)

dic <- data.frame(
  item_name = names(dat),
  item_label = c(item_names, "gender", "age"),
  scale = c(rep(c("rel", "sui"), each = 5), rep("misc",2)),
  scale_label = c(rep(c("Religious beliefs", "Suicide tendency"), each = 5), rep("Miscellaneous",2)),
  values = c(rep("1:6", 5), rep("0:4", 5), "'m', 'f', 'ka'", "5:11"),
  value_labels = c(value_labels, "m = male; f = female; d = diverse", ""),
  type = c(rep("integer", n_var), "factor", "integer"),
  weight = 1,
  missing = c(rep("-999", n_var), "", "-999")
)


## ----echo=FALSE---------------------------------------------------------------
dic %>% select(1:2) %>% kable(caption = "Dictionary file")#rmarkdown::paged_table()

## ----include = FALSE----------------------------------------------------------
dic_file <- dic[,1:2]

## -----------------------------------------------------------------------------
dat_dic <- apply_dic(dat, dic_file)
dat_dic <- check_values(dat_dic, report = TRUE)
replace_missing(dat_dic)

## ----include = FALSE----------------------------------------------------------
dic_file <- dic[,1:2]

## -----------------------------------------------------------------------------
dat_dic <- apply_dic(dat, dic_file)

## ----include = FALSE----------------------------------------------------------
dic_file <- dic %>% select(1,2,"weight", "type")

## ----echo=FALSE---------------------------------------------------------------
dic_file %>% kable(caption = "Dictionary file")#rmarkdown::paged_table()

