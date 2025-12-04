

library(scaledic)

dat <- new_dic(
  c(1,1,2,3,1,3,4,4,3,2,4),
  item_name = "knowledge_1",
  item_label = "What is the Captial of Germany?",
  type = "integer",
  weight = 1,
  values = "1:4",
  value_labels = "1 = Brussels; 2 = Hamburg; 3 = Bonn; 4 = Berlin",
  scores = "4 = 1; .default = 0"
)
dat
recode_dic_items(dat)



scaledic:::string_to_list("0 = 0; 2 = 1; .default = 99; 4 = 0")



# ----------------------

my::file_path("sciebo", "tmp", "bodo") |> dir()

library(readr)
dat <- readr::read_csv("~/sciebo/tmp/bodo/survey_472834_R_data_file Kopie.csv")

View(dat)

dic <- readr::read_delim("~/sciebo/tmp/bodo/survey_472834_R_syntax_file_DICTIONARY_v2.csv",
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

apply_dic(dat, dic)


new_dic(
  1:10,
  values = NA,
  value_labels = "00=Ich habe keinen Code erhalten; 01=1; 02=2; 03=3; 04=4; 05=5; 06=6; 07=7; 08=8; 09=9; 10=10; 11=11; 12=12; 13=13; 14=14; 15=15; 16=16; 17=17; 18=18; 19=19; 20=20; 21=21; 22=22; 23=23; 24=24; 25=25; 26=26; 27=27; 28=28; 29=29; 30=30; 31=31; 32=32; 33=33; 34=34; 35=35; 36=36; 37=37; 38=38; 39=39; 40=40; 41=41; 42=42; 43=43; 44=44; 45=45",
  type = "factor"

)


new_dic(
  1:10,
  values = c("01", "02", "03", "04", "05"),
  value_labels = "01=Ich habe keinen Code erhalten; 02=2; 03=3; 04=4; 05=5",
  type = "factor"

)



# ---------------------

print_vector_custom <- function(x, width = getOption("width")) {
  x <- as.character(x)
  lines <- character()
  current_line <- "| "
  current_width <- 2  # account for "| "

  for (elem in x) {
    elem_width <- nchar(elem) + 1  # add 1 for the space
    if (current_width + elem_width > width) {
      lines <- c(lines, current_line)
      current_line <- paste0("| ", elem, " ")
      current_width <- nchar(current_line)
    } else {
      current_line <- paste0(current_line, elem, " ")
      current_width <- current_width + elem_width
    }
  }
  lines <- c(lines, current_line)
  cat(paste0(lines, "\n"), sep = "")
}
v <- 1:100
print_vector_custom(v)


# -----------------


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

