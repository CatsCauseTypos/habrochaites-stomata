# 9/28/2017 just grabbing a table of the unique LA#'s from the hab experiment

data_qck <- read.csv("~/Desktop/Sinha_Lab_stuff/hab-epidermis/Data/R_HAPPY_FORMAT_hab.csv") # full version
head(data_qck)
ILs <- unique(data_qck$IL)

write.csv(ILs, "unique_ILs.csv")


