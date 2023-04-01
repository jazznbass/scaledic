
dic_itrf <- openxlsx::read.xlsx(file.path("data-raw", "dic_itrf.xlsx"))
dat_itrf <- openxlsx::read.xlsx(file.path("data-raw", "itrf.xlsx"))
ex_itrf <- apply_dic(dat_itrf, dic_itrf)

create_norms <- function(raw) {
  normtable <- data.frame(
    group = "all",
    raw = unique(raw) |> sort()
  )
  M <- mean(raw, na.rm = TRUE)
  S <- sd(raw, na.rm = TRUE)
  normtable$T <- round((normtable$raw - M) / S * 10 + 50)

  PR <-sapply(normtable$raw, function(x) mean(raw <= x, na.rm = TRUE))
  normtable$PR <- round(PR*100)
  normtable$T_from_PR <- round(qnorm(PR) * 10 + 50)
  normtable
}

ex_normtable_int <- create_norms(score_scale(ex_itrf, subscale == "Int", sum = TRUE, max_na = 0)  )
ex_normtable_ext <- create_norms(score_scale(ex_itrf, subscale == "Ext", sum = TRUE, max_na = 0)  )


usethis::use_data(
  ex_itrf,
  dic_itrf,
  dat_itrf,
  ex_normtable_int,
  ex_normtable_ext,
  overwrite = TRUE
)




