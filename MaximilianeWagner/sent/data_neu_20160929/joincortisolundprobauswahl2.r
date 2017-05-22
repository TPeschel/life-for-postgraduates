library(readxl)
setwd("C:/Users/Anwender/Documents/Dissertation/PV208/data/data_neu_20160929")
t1 <- read_excel("C:/Users/Anwender/Documents/Dissertation/PV208/data/data_neu_20160929/20161110_Probenliste_AGa (1).xlsx")
t2 <- read_excel("C:/Users/Anwender/Documents/Dissertation/PV208/data/data_neu_20160929/PV208_Probauswahl2_20160929.xlsx")
t3 <- merge( t1,t2,by = "Materialnummer", all = T)
names(t3)
view(t3)
t2[t2$Materialnummer=="A26F3BHA",]


