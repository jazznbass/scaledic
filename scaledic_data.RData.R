library(openxlsx)

dic_ITRF <- read.xlsx("dic_itrf.xlsx")
ITRF <- read.xlsx("itrf.xlsx")
names(ITRF)

save(ITRF, dic_ITRF, file = "data/scaledic.RData")


