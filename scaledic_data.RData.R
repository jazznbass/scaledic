
dic_ITRF <- openxlsx::read.xlsx("dic_itrf.xlsx")
ITRF <- openxlsx::read.xlsx("itrf.xlsx")

save(ITRF, dic_ITRF, file = "data/scaledic.RData")


