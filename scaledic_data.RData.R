
dic_ITRF <- openxlsx::read.xlsx("dic_itrf.xlsx")
ITRF <- openxlsx::read.xlsx("itrf.xlsx")
ex_itrf <- apply_dic(ITRF, dic_ITRF)
save(ex_itrf, dic_ITRF, file = "data/scaledic.RData")


