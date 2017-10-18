library(helpR)
library(lubridate)
library(ggplot2)

tbls <-
  read.all.xlsx.tables( "../../data/" )

t.n <-
  all.xlsx.tables.names( tbls )

t.c.n <- 
  all.xlsx.tables.row.names( tbls )

t.c.n

t.c.n[2]

t.n[2]

t.n[14]

d00127 <- get.tbl( "D00127", all.tables = tbls )
t00213 <- get.tbl("T00213", all.tables = tbls)

focus.asthma <- merge(
  get.tbl(tbl.name = "D00127", all.tables = tbls),
  get.tbl(tbl.name = "T00213", all.tables = tbls),
  by.x = c("C_DISEASE_TX_SIC"),
  by.y = c("FB_ALLERGY_BP1_SIC"),
  all=F
)

table(focus.asthma$C_DISEASE_TX_ASTHMA)

asthma <- focus.asthma[ !is.na(focus.asthma$C_DISEASE_TX_ASTHMA)
                        & focus.asthma$C_DISEASE_TX_ASTHMA == 1
                        &focus.asthma$FB_ALLERGY_BP1_F0041 == 1,
                        c( "C_DISEASE_TX_SIC", "C_DISEASE_TX_ASTHMA",
                           "FB_ALLERGY_BP1_F0041")]
## Es gibt 28 Kinder, die im C_DISEASE_TX_ASTHMA Asthma als bekannte
#  Krankheit angeben und deren Eltern im Fragebogen von Atembeschwerden berichten

asthma.2 <- focus.asthma[ !is.na(focus.asthma$C_DISEASE_TX_ASTHMA)
                        & focus.asthma$C_DISEASE_TX_ASTHMA == 1
                        & focus.asthma$FB_ALLERGY_BP1_F0041 == 1
                        & focus.asthma$FB_ALLERGY_BP1_F0043 == 1,
                        c( "C_DISEASE_TX_SIC", "C_DISEASE_TX_ASTHMA",
                           "FB_ALLERGY_BP1_F0041","FB_ALLERGY_BP1_F0043")]
summary(asthma.2)

## Es gibt 18 Kinder, die im C_DISEASE_TX_ASTHMA Asthma als bekannte
#  Krankheit angeben und deren Eltern im Fragebogen von Atembeschwerden
#  und einer diagnostizierten Asthmaerkrankung berichten

table(d00127$C_DISEASE_TX_ASTHMA)

## Im C_DISEASE_TX_ASTHMA werden 357 Asthmakinder angezeigt.
