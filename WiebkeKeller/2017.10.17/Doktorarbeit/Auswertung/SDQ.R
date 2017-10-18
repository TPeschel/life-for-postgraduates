setwd("C:/Users/Wiebke")
SDQ <- read_excel("Desktop/Doktorarbeit/PV332/data/PV0332_D00148_NODUP.xlsx")
table(SDQ$SDQ_GES_SCORE)
summary(SDQ$SDQ_GES_SCORE)
SDQ[SDQ$SDQ_GES_SCORE > 17,]
table(SDQ$SDQ_GES_SCORE > 17)
204/2349
ggplot(SDQ, aes(x=SDQ$SDQ_SCI_GROUP,y=SDQ$SDQ_GES_SCORE))+geom_boxplot()

Stammdaten <- read_excel("Desktop/Doktorarbeit/PV332/data/PV0332_R00001.xlsx")
Stammdaten$TEILNEHMER_GESCHLECHT.mf <- factor(Stammdaten$TEILNEHMER_GESCHLECHT, levels = 1:2, labels = c("male","female"))
table(Stammdaten$TEILNEHMER_GESCHLECHT.mf)

Allergie_Eltern <- read_excel("Desktop/Doktorarbeit/PV332/data/PV0332_T00213_NODUP.xlsx")
table(Allergie_Eltern$FB_ALLERGY_BP1_F0029)

SDQ[SDQ$SDQ_GES_SCORE > 17,]
table(SDQ$SDQ_SIC, SDQ$SDQ_GES_SCORE > 17)
Allergie_Eltern[Allergie_Eltern$FB_ALLERGY_BP1_F0029]
