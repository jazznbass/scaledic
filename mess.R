
library(tidyverse)
library(haven)
library(openxlsx)
library(readxl)
if (!("scaledic" %in% installed.packages())) devtools::install_github("jazznbass/scaledic")
library(scaledic)
library(nlme)
library(lme4)
library(lmerTest)



# dirs --------------------------------------------------------------------

dir <- list(root = file.path(my_dir("project", "erep"), "datasets", "uwezo"))
dir$project <- file.path(dir$root, "data", "preparation")
dir$raw <- file.path(dir$root, "data", "raw", "2018")
dir$clean <- file.path(dir$root, "data", "clean", "2018")


# import data -------------------------------------------------------------

setwd(dir$raw)

dat_hh <- read_stata("hhld_UG2018.dta")
dat_vl <- read_stata("village_UG2018.dta")
dat_sc <- read_stata("school_UG2018.dta" )
dat_ch <- read_stata("child_UG2018.dta")


dat_hh$hh_id <- as.character(dat_hh$h002)
dat_ch$hh_id <- paste0(dat_ch$vid, dat_ch$h001)


dat_18 <- full_join(dat_ch, dat_hh, by = "hh_id", suffix = c("", ".hh"))
dat_18 <- full_join(dat_18, dat_vl, by = "vid", suffix = c("", ".vl"))
dat_18 <- full_join(dat_18, dat_sc, by = "vid", suffix = c("", ".sc"))

dat_18 <- haven_dic(dat_18)

dat_18$h500class_level <- dic(dat_18$h500class_level, item_label = "class level")

dat_18 <- dat_18 %>%
  arrange(hh_id, srno) %>%
  relocate(hh_id, srno)



###


scales <- get_scales(ex_itrf,
  'APD' = subscale_2 == "APD",
  'OPP' = subscale_2 == "OPP",
  "SW" = subscale_2 == "SW",
  "AD" = subscale_2 == "AD"
)

scale_names <- names(scales)

names(scales) <- rep("filter", length(scales))

scales <- c(data = list(dat), scales[i], names_only = TRUE)


do.call("select_items", scales)


filter <- deparse(substitute(filter))

id <- .get_index(data = data, filter = filter, class = "item", names = FALSE)
if (names_only) return(names(data)[id])

do.call("scaledic:::-get_index",
        list(data data = scales
        )

scales

