
dic_itrf <- openxlsx::read.xlsx("dic_itrf.xlsx")
dat_itrf <- openxlsx::read.xlsx("itrf.xlsx")
ex_itrf <- apply_dic(dat_itrf, dic_itrf)
save(ex_itrf, dic_itrf, dat_itrf, file = "data/scaledic.RData", compress = "xz")


