##Skript: Gruppe 1 (tbl.disease.gruppe1), in der ich die disease/Medi-flags aussortiert habe. 
# 1. Krankheiten/Medis/fehldener PubStat: NAs zuweisen, z‰hlen, protokollieren
# 2. Pubstat ausschlieﬂen, Krankheiten, Medis, Freitexte, erste Analysen, dann Mehrfachbesuche ausschlieﬂen

rm( list = ls( ) ) ##Speicher l√∂schenren
library(ggplot2)
library(reshape2) #u.a. melt funktion
library(readxl) ##standart excel tbl lesen
library( dplyr ) ##group by befehl, summarise,...
library( openxlsx ) ##lesen und schreiben
library(xlsx) ##lesen und schreiben
library(WriteXLS) ##standart zum schreiben
library(psych) ##f¸r describeBY
my.theme <-  # bevorzugtes Schema fuer Grafiken
  theme_bw( )

setwd("c:/Users/Lea/Desktop/DA/LIFE-Daten/R-Projekte/r20170601/") ##Pfad setzen

load("data/tbl.weniger.spalten1.Rd")  ##Daten laden

tbl.disease.gruppe1 <- tbl.weniger.spalten1 ##neue Tabelle benennen
nrow(tbl.disease.gruppe1) ## 2907 

## 1. NAs auf 0, dann protokollieren
# Krankheiten:
#   SD_Allg, SD_Hypo, Sd_Hyper, Fruehgeb., Endokr, DM1, DM2, NGScreen, Blut, Gerinnung, Pulmo, Asthma, 
#   GIT, Infekt, HWI, Nierenfehl, Kardio_Rhyth, Kardio, Allergie, Epikrampf, Psy, ADHS, Depres, Sucht, Muskel

tbl.disease.gruppe1$C_DISEASE_TX_SD_ALLG[is.na(tbl.disease.gruppe1$C_DISEASE_TX_SD_ALLG)] <- 0
tbl.disease.gruppe1$C_DISEASE_TX_SD_HYPO[is.na(tbl.disease.gruppe1$C_DISEASE_TX_SD_HYPO)] <- 0
tbl.disease.gruppe1$C_DISEASE_TX_SD_HYPER[is.na(tbl.disease.gruppe1$C_DISEASE_TX_SD_HYPER)] <- 0
tbl.disease.gruppe1$C_DISEASE_TX_FRUEHGEB[is.na(tbl.disease.gruppe1$C_DISEASE_TX_FRUEHGEB)] <- 0
tbl.disease.gruppe1$C_DISEASE_TX_ENDOKR[is.na(tbl.disease.gruppe1$C_DISEASE_TX_ENDOKR)] <- 0
tbl.disease.gruppe1$C_DISEASE_TX_DM1[is.na(tbl.disease.gruppe1$C_DISEASE_TX_DM1)] <- 0
tbl.disease.gruppe1$C_DISEASE_TX_DM2[is.na(tbl.disease.gruppe1$C_DISEASE_TX_DM2)] <- 0
tbl.disease.gruppe1$C_DISEASE_TX_NGSCREEN[is.na(tbl.disease.gruppe1$C_DISEASE_TX_NGSCREEN)] <- 0
tbl.disease.gruppe1$C_DISEASE_TX_BLUT[is.na(tbl.disease.gruppe1$C_DISEASE_TX_BLUT)] <- 0
tbl.disease.gruppe1$C_DISEASE_TX_GERIN[is.na(tbl.disease.gruppe1$C_DISEASE_TX_GERIN)] <- 0
tbl.disease.gruppe1$C_DISEASE_TX_PULMO[is.na(tbl.disease.gruppe1$C_DISEASE_TX_PULMO)] <- 0
tbl.disease.gruppe1$C_DISEASE_TX_ASTHMA[is.na(tbl.disease.gruppe1$C_DISEASE_TX_ASTHMA)] <- 0
tbl.disease.gruppe1$C_DISEASE_TX_GIT[is.na(tbl.disease.gruppe1$C_DISEASE_TX_GIT)] <- 0
tbl.disease.gruppe1$C_DISEASE_TX_INFEKT[is.na(tbl.disease.gruppe1$C_DISEASE_TX_INFEKT)] <- 0
tbl.disease.gruppe1$C_DISEASE_TX_HWI_PN[is.na(tbl.disease.gruppe1$C_DISEASE_TX_HWI_PN)] <- 0
tbl.disease.gruppe1$C_DISEASE_TX_NIERENFEHL[is.na(tbl.disease.gruppe1$C_DISEASE_TX_NIERENFEHL)] <- 0
tbl.disease.gruppe1$C_DISEASE_TX_KARDIO_RHYTH[is.na(tbl.disease.gruppe1$C_DISEASE_TX_KARDIO_RHYTH)] <- 0
tbl.disease.gruppe1$C_DISEASE_TX_KARDIO[is.na(tbl.disease.gruppe1$C_DISEASE_TX_KARDIO)] <- 0
tbl.disease.gruppe1$C_DISEASE_TX_ALLERG[is.na(tbl.disease.gruppe1$C_DISEASE_TX_ALLERG)] <- 0
tbl.disease.gruppe1$C_DISEASE_TX_EPIKRAMPF[is.na(tbl.disease.gruppe1$C_DISEASE_TX_EPIKRAMPF)] <- 0
tbl.disease.gruppe1$C_DISEASE_TX_PSY[is.na(tbl.disease.gruppe1$C_DISEASE_TX_PSY)] <- 0
tbl.disease.gruppe1$C_DISEASE_TX_ADHS[is.na(tbl.disease.gruppe1$C_DISEASE_TX_ADHS)] <- 0
tbl.disease.gruppe1$C_DISEASE_TX_DEPRES[is.na(tbl.disease.gruppe1$C_DISEASE_TX_DEPRES)] <- 0
tbl.disease.gruppe1$C_DISEASE_TX_SUCHT[is.na(tbl.disease.gruppe1$C_DISEASE_TX_SUCHT)] <- 0
tbl.disease.gruppe1$C_DISEASE_TX_MUSKEL[is.na(tbl.disease.gruppe1$C_DISEASE_TX_MUSKEL)] <- 0

# Krankheiten: 2.Flags z√§hlen und protokollieren
sum(tbl.disease.gruppe1$C_DISEASE_TX_SD_ALLG == 1) 
sum(tbl.disease.gruppe1$C_DISEASE_TX_SD_HYPO == 1) ##23
sum(tbl.disease.gruppe1$C_DISEASE_TX_SD_HYPER == 1) ##2
sum(tbl.disease.gruppe1$C_DISEASE_TX_FRUEHGEB == 1) ##1
sum(tbl.disease.gruppe1$C_DISEASE_TX_ENDOKR == 1) 
sum(tbl.disease.gruppe1$C_DISEASE_TX_DM1 == 1) 
sum(tbl.disease.gruppe1$C_DISEASE_TX_DM2 == 1) 
# sum(tbl.disease.gruppe1$C_DISEASE_TX_NGSCREEN == 1) ##0
sum(tbl.disease.gruppe1$C_DISEASE_TX_BLUT == 1) 
sum(tbl.disease.gruppe1$C_DISEASE_TX_GERIN == 1) 
sum(tbl.disease.gruppe1$C_DISEASE_TX_PULMO == 1) 
sum(tbl.disease.gruppe1$C_DISEASE_TX_ASTHMA == 1) 
sum(tbl.disease.gruppe1$C_DISEASE_TX_GIT == 1) 
sum(tbl.disease.gruppe1$C_DISEASE_TX_INFEKT == 1) 
sum(tbl.disease.gruppe1$C_DISEASE_TX_HWI_PN == 1) 
sum(tbl.disease.gruppe1$C_DISEASE_TX_NIERENFEHL == 1) 
sum(tbl.disease.gruppe1$C_DISEASE_TX_KARDIO_RHYTH == 1) 
sum(tbl.disease.gruppe1$C_DISEASE_TX_KARDIO == 1) 
sum(tbl.disease.gruppe1$C_DISEASE_TX_ALLERG == 1) 
sum(tbl.disease.gruppe1$C_DISEASE_TX_EPIKRAMPF == 1) 
sum(tbl.disease.gruppe1$C_DISEASE_TX_PSY == 1) 
sum(tbl.disease.gruppe1$C_DISEASE_TX_ADHS == 1) 
sum(tbl.disease.gruppe1$C_DISEASE_TX_DEPRES == 1) 
sum(tbl.disease.gruppe1$C_DISEASE_TX_SUCHT == 1)
sum(tbl.disease.gruppe1$C_DISEASE_TX_MUSKEL == 1) 

protokoll.Krankheit <-
  data.frame( 
    Krankheit = as.character( c( "SD_Allg", "SD_Hypo", "Sd_Hyper", "Fruehgeb", "Endokr", "DM1", "DM2", "Blut", "Gerinnung", "Pulmo", "Asthma", 
                                 "GIT", "Infekt", "HWI", "Nierenfehl", "Kardio_Rhyth", "Kardio", "Allergie", "Epikrampf", "Psy", 
                                 "ADHS", "Depres", "Sucht", "Muskel")),
    Anzahl = c(1:24)
  )

protokoll.Krankheit$Anzahl[ protokoll.Krankheit$Krankheit=="SD_Allg" ] <- sum(tbl.disease.gruppe1$C_DISEASE_TX_SD_ALLG == 1)
protokoll.Krankheit$Anzahl[ protokoll.Krankheit$Krankheit=="SD_Hypo" ] <- sum(tbl.disease.gruppe1$C_DISEASE_TX_SD_HYPO == 1)
protokoll.Krankheit$Anzahl[ protokoll.Krankheit$Krankheit=="SD_Hyper" ] <- sum(tbl.disease.gruppe1$C_DISEASE_TX_SD_HYPER == 1)
protokoll.Krankheit$Anzahl[ protokoll.Krankheit$Krankheit=="Fruehgeb" ] <- sum(tbl.disease.gruppe1$C_DISEASE_TX_FRUEHGEB == 1)
protokoll.Krankheit$Anzahl[ protokoll.Krankheit$Krankheit=="Endokr" ] <- sum(tbl.disease.gruppe1$C_DISEASE_TX_ENDOKR == 1)
protokoll.Krankheit$Anzahl[ protokoll.Krankheit$Krankheit=="DM1" ] <- sum(tbl.disease.gruppe1$C_DISEASE_TX_DM1 == 1)
protokoll.Krankheit$Anzahl[ protokoll.Krankheit$Krankheit=="DM2" ] <- sum(tbl.disease.gruppe1$C_DISEASE_TX_DM2 == 1)
protokoll.Krankheit$Anzahl[ protokoll.Krankheit$Krankheit=="Blut" ] <- sum(tbl.disease.gruppe1$C_DISEASE_TX_BLUT == 1)
protokoll.Krankheit$Anzahl[ protokoll.Krankheit$Krankheit=="Gerin" ] <- sum(tbl.disease.gruppe1$C_DISEASE_TX_GERIN == 1)
protokoll.Krankheit$Anzahl[ protokoll.Krankheit$Krankheit=="Pulmo" ] <- sum(tbl.disease.gruppe1$C_DISEASE_TX_PULMO == 1)
protokoll.Krankheit$Anzahl[ protokoll.Krankheit$Krankheit=="Asthma" ] <- sum(tbl.disease.gruppe1$C_DISEASE_TX_ASTHMA == 1)
protokoll.Krankheit$Anzahl[ protokoll.Krankheit$Krankheit=="GIT" ] <- sum(tbl.disease.gruppe1$C_DISEASE_TX_GIT == 1)
protokoll.Krankheit$Anzahl[ protokoll.Krankheit$Krankheit=="Infekt" ] <- sum(tbl.disease.gruppe1$C_DISEASE_TX_INFEKT == 1)
protokoll.Krankheit$Anzahl[ protokoll.Krankheit$Krankheit=="HWI" ] <- sum(tbl.disease.gruppe1$C_DISEASE_TX_HWI_PN == 1)
protokoll.Krankheit$Anzahl[ protokoll.Krankheit$Krankheit=="Nierenfehl" ] <- sum(tbl.disease.gruppe1$C_DISEASE_TX_NIERENFEHL == 1)
protokoll.Krankheit$Anzahl[ protokoll.Krankheit$Krankheit=="Kardio_Rhyth" ] <- sum(tbl.disease.gruppe1$C_DISEASE_TX_KARDIO_RHYTH == 1)
protokoll.Krankheit$Anzahl[ protokoll.Krankheit$Krankheit=="Kardio" ] <- sum(tbl.disease.gruppe1$C_DISEASE_TX_KARDIO == 1)
protokoll.Krankheit$Anzahl[ protokoll.Krankheit$Krankheit=="Allergie" ] <- sum(tbl.disease.gruppe1$C_DISEASE_TX_ALLERG == 1)
protokoll.Krankheit$Anzahl[ protokoll.Krankheit$Krankheit=="Epikrampf" ] <- sum(tbl.disease.gruppe1$C_DISEASE_TX_EPIKRAMPF == 1)
protokoll.Krankheit$Anzahl[ protokoll.Krankheit$Krankheit=="Psy" ] <- sum(tbl.disease.gruppe1$C_DISEASE_TX_PSY == 1)
protokoll.Krankheit$Anzahl[ protokoll.Krankheit$Krankheit=="ADHS" ] <- sum(tbl.disease.gruppe1$C_DISEASE_TX_ADHS == 1)
protokoll.Krankheit$Anzahl[ protokoll.Krankheit$Krankheit=="Depres" ] <- sum(tbl.disease.gruppe1$C_DISEASE_TX_DEPRES == 1)
protokoll.Krankheit$Anzahl[ protokoll.Krankheit$Krankheit=="Sucht" ] <- sum(tbl.disease.gruppe1$C_DISEASE_TX_SUCHT == 1)
protokoll.Krankheit$Anzahl[ protokoll.Krankheit$Krankheit=="Muskel" ] <- sum(tbl.disease.gruppe1$C_DISEASE_TX_MUSKEL == 1)

protokoll.Krankheit  #Protokoll erstellen

# MEDIS: NAs und z‰hlen, protokollieren
tbl.disease.gruppe1$CHILD_MED_H_GLUCO_CORT[is.na(tbl.disease.gruppe1$CHILD_MED_H_GLUCO_CORT)] <- 0
tbl.disease.gruppe1$CHILD_MED_H_ASTHMA[is.na(tbl.disease.gruppe1$CHILD_MED_H_ASTHMA)] <- 0
tbl.disease.gruppe1$CHILD_MED_H_WACHSTUM[is.na(tbl.disease.gruppe1$CHILD_MED_H_WACHSTUM)] <- 0
tbl.disease.gruppe1$CHILD_MED_H_IMMUNSUPP[is.na(tbl.disease.gruppe1$CHILD_MED_H_IMMUNSUPP)] <- 0
tbl.disease.gruppe1$CHILD_MED_H_NEUROLEPTIKA[is.na(tbl.disease.gruppe1$CHILD_MED_H_NEUROLEPTIKA)] <- 0
tbl.disease.gruppe1$CHILD_MED_H_ANTIPSYCH[is.na(tbl.disease.gruppe1$CHILD_MED_H_ANTIPSYCH)] <- 0
tbl.disease.gruppe1$CHILD_MED_H_ADHS[is.na(tbl.disease.gruppe1$CHILD_MED_H_ADHS)] <- 0
tbl.disease.gruppe1$CHILD_MED_H_HORMONE[is.na(tbl.disease.gruppe1$CHILD_MED_H_HORMONE)] <- 0
tbl.disease.gruppe1$CHILD_MED_H_STOFFWECHSEL[is.na(tbl.disease.gruppe1$CHILD_MED_H_STOFFWECHSEL)] <- 0
tbl.disease.gruppe1$CHILD_MED_H_METFORMIN[is.na(tbl.disease.gruppe1$CHILD_MED_H_METFORMIN)] <- 0
tbl.disease.gruppe1$CHILD_MED_H_INSULIN[is.na(tbl.disease.gruppe1$CHILD_MED_H_INSULIN)] <- 0
tbl.disease.gruppe1$CHILD_MED_H_LTHYROX[is.na(tbl.disease.gruppe1$CHILD_MED_H_LTHYROX)] <- 0
tbl.disease.gruppe1$CHILD_MED_H_KONTRAZEPT[is.na(tbl.disease.gruppe1$CHILD_MED_H_KONTRAZEPT)] <- 0
tbl.disease.gruppe1$CHILD_MED_H_SEX_STEROIDE[is.na(tbl.disease.gruppe1$CHILD_MED_H_SEX_STEROIDE)] <- 0
tbl.disease.gruppe1$CHILD_MED_H_TESTO[is.na(tbl.disease.gruppe1$CHILD_MED_H_TESTO)] <- 0
tbl.disease.gruppe1$CHILD_MED_H_ANTIBIO[is.na(tbl.disease.gruppe1$CHILD_MED_H_ANTIBIO)] <- 0
tbl.disease.gruppe1$CHILD_MED_H_ALLERGIE[is.na(tbl.disease.gruppe1$CHILD_MED_H_ALLERGIE)] <- 0
tbl.disease.gruppe1$CHILD_MED_H_ANTIHIST[is.na(tbl.disease.gruppe1$CHILD_MED_H_ANTIHIST)] <- 0
tbl.disease.gruppe1$CHILD_MED_H_HYPOSENS[is.na(tbl.disease.gruppe1$CHILD_MED_H_HYPOSENS)] <- 0
tbl.disease.gruppe1$CHILD_MED_H_MINERALOCORT[is.na(tbl.disease.gruppe1$CHILD_MED_H_MINERALOCORT)] <- 0

sum(tbl.disease.gruppe1$CHILD_MED_H_GLUCO_CORT == 1) ##155 Glukokortikoide
sum(tbl.disease.gruppe1$CHILD_MED_H_ASTHMA == 1) #173 (viele davon sind Glukokortikoide)
sum(tbl.disease.gruppe1$CHILD_MED_H_WACHSTUM == 1)  #8 Wachstumshormone
sum(tbl.disease.gruppe1$CHILD_MED_H_IMMUNSUPP == 1) #11
sum(tbl.disease.gruppe1$CHILD_MED_H_NEUROLEPTIKA == 1) #59
sum(tbl.disease.gruppe1$CHILD_MED_H_ANTIPSYCH == 1) #3
sum(tbl.disease.gruppe1$CHILD_MED_H_ADHS == 1) #45
sum(tbl.disease.gruppe1$CHILD_MED_H_HORMONE == 1) # 135
sum(tbl.disease.gruppe1$CHILD_MED_H_STOFFWECHSEL == 1) #21
sum(tbl.disease.gruppe1$CHILD_MED_H_METFORMIN == 1) #12
sum(tbl.disease.gruppe1$CHILD_MED_H_INSULIN == 1) #4
sum(tbl.disease.gruppe1$CHILD_MED_H_LTHYROX == 1) # 67
sum(tbl.disease.gruppe1$CHILD_MED_H_KONTRAZEPT == 1) #80
sum(tbl.disease.gruppe1$CHILD_MED_H_SEX_STEROIDE == 1) #99
sum(tbl.disease.gruppe1$CHILD_MED_H_TESTO == 1) #2
sum(tbl.disease.gruppe1$CHILD_MED_H_ANTIBIO == 1) #40
sum(tbl.disease.gruppe1$CHILD_MED_H_ALLERGIE == 1) #151
sum(tbl.disease.gruppe1$CHILD_MED_H_ANTIHIST == 1) #108
sum(tbl.disease.gruppe1$CHILD_MED_H_HYPOSENS == 1) #54
sum(tbl.disease.gruppe1$CHILD_MED_H_MINERALOCORT == 1) #2

# tbl.disease.gruppe1$CHILD_MED_H_MAGDRAM[is.na(tbl.disease.gruppe1$CHILD_MED_H_MAGDRAM)] <- 0
# sum(tbl.disease.gruppe1$CHILD_MED_H_MAGDRAM == 1) # 33 --> schlie√üe ich nicht aus
# tbl.disease.gruppe1$CHILD_MED_H_UROLOGIE[is.na(tbl.disease.gruppe1$CHILD_MED_H_UROLOGIE)] <- 0
# sum(tbl.disease.gruppe1$CHILD_MED_H_UROLOGIE == 1) #5 --> nicht ausschlie√üen, meist gegen einn√§ssen
# tbl.disease.gruppe1$CHILD_MED_H_ATEM_STIM[is.na(tbl.disease.gruppe1$CHILD_MED_H_ATEM_STIM)] <- 0
# sum(tbl.disease.gruppe1$CHILD_MED_H_ATEM_STIM == 1) #0 --> nicht beachten
# tbl.disease.gruppe1$CHILD_MED_H_DESMOPRESS[is.na(tbl.disease.gruppe1$CHILD_MED_H_DESMOPRESS)] <- 0
# sum(tbl.disease.gruppe1$CHILD_MED_H_DESMOPRESS == 1) #0 --> Desmopressiva muss ich nicht beachten

protokoll.Medikamente <-
  data.frame( 
    Medikamente = as.character( c( "Gluko", "Asthma", "Wachstumshormon", "Immnunsuppr", "Neuroleptika", "Antipsychotika", "ADHS", "Hormone", "Stoffwechsel",
                                   "Metformin", "Insulin", "L-Tyroxin", "Kontrazeptiva", "Sexualsteroide", "Testosteron", "Antibiotika", "Allergie", 
                                   "Antihis", "Hyposensibilisierung", "Mineralocorticoide")),
    Anzahl = c(1:20)
  )
protokoll.Medikamente$Anzahl[ protokoll.Medikamente$Medikament=="Gluko" ] <- sum(tbl.disease.gruppe1$CHILD_MED_H_GLUCO_CORT == 1)
protokoll.Medikamente$Anzahl[ protokoll.Medikamente$Medikament=="Asthma" ] <- sum(tbl.disease.gruppe1$CHILD_MED_H_ASTHMA == 1)
protokoll.Medikamente$Anzahl[ protokoll.Medikamente$Medikament=="Wachstumshormon" ] <- sum(tbl.disease.gruppe1$CHILD_MED_H_WACHSTUM == 1)
protokoll.Medikamente$Anzahl[ protokoll.Medikamente$Medikament=="Immunsuppr" ] <- sum(tbl.disease.gruppe1$CHILD_MED_H_IMMUNSUPP == 1)
protokoll.Medikamente$Anzahl[ protokoll.Medikamente$Medikament=="Neuroleptika" ] <- sum(tbl.disease.gruppe1$CHILD_MED_H_NEUROLEPTIKA == 1)
protokoll.Medikamente$Anzahl[ protokoll.Medikamente$Medikament=="Antipsychotika" ] <- sum(tbl.disease.gruppe1$CHILD_MED_H_ANTIPSYCH == 1)
protokoll.Medikamente$Anzahl[ protokoll.Medikamente$Medikament=="ADHS" ] <- sum(tbl.disease.gruppe1$CHILD_MED_H_ADHS == 1)
protokoll.Medikamente$Anzahl[ protokoll.Medikamente$Medikament=="Hormone" ] <- sum(tbl.disease.gruppe1$CHILD_MED_H_HORMONE == 1)
protokoll.Medikamente$Anzahl[ protokoll.Medikamente$Medikament=="Stwoffwechsel" ] <- sum(tbl.disease.gruppe1$CHILD_MED_H_STOFFWECHSEL == 1)
protokoll.Medikamente$Anzahl[ protokoll.Medikamente$Medikament=="Metformon" ] <- sum(tbl.disease.gruppe1$CHILD_MED_H_METFORMIN == 1)
protokoll.Medikamente$Anzahl[ protokoll.Medikamente$Medikament=="Insulin" ] <- sum(tbl.disease.gruppe1$CHILD_MED_H_INSULIN == 1)
protokoll.Medikamente$Anzahl[ protokoll.Medikamente$Medikament=="L-Thyroxin" ] <- sum(tbl.disease.gruppe1$CHILD_MED_H_LTHYROX == 1)
protokoll.Medikamente$Anzahl[ protokoll.Medikamente$Medikament=="Kontrazeptiva" ] <- sum(tbl.disease.gruppe1$CHILD_MED_H_KONTRAZEPT == 1)
protokoll.Medikamente$Anzahl[ protokoll.Medikamente$Medikament=="Sexulsteroide" ] <- sum(tbl.disease.gruppe1$CHILD_MED_H_SEX_STEROIDE == 1)
protokoll.Medikamente$Anzahl[ protokoll.Medikamente$Medikament=="Testosteron" ] <- sum(tbl.disease.gruppe1$CHILD_MED_H_TESTO == 1)
protokoll.Medikamente$Anzahl[ protokoll.Medikamente$Medikament=="Antibiotika" ] <- sum(tbl.disease.gruppe1$CHILD_MED_H_ANTIBIO == 1)
protokoll.Medikamente$Anzahl[ protokoll.Medikamente$Medikament=="Allergie" ] <- sum(tbl.disease.gruppe1$CHILD_MED_H_ALLERGIE == 1)
protokoll.Medikamente$Anzahl[ protokoll.Medikamente$Medikament=="Antihis" ] <- sum(tbl.disease.gruppe1$CHILD_MED_H_ANTIHIST == 1)
protokoll.Medikamente$Anzahl[ protokoll.Medikamente$Medikament=="Hyposens" ] <- sum(tbl.disease.gruppe1$CHILD_MED_H_HYPOSENS == 1)
protokoll.Medikamente$Anzahl[ protokoll.Medikamente$Medikament=="Mineralocortirticoide" ] <- sum(tbl.disease.gruppe1$CHILD_MED_H_MINERALOCORT == 1)

protokoll.Medikamente

##PubStat aussortieren 
tbl.disease.gruppe1$C_PUB_STAT_PUB_STATUS[is.na(tbl.disease.gruppe1$C_PUB_STAT_PUB_STATUS)] <- 0
sum(tbl.disease.gruppe1$C_PUB_STAT_PUB_STATUS == 0) ##646 fehlende PubStat
tbl.disease.gruppe1 <- tbl.disease.gruppe1[tbl.disease.gruppe1$C_PUB_STAT_PUB_STATUS != 0,] # ungleich 0= (!=), wir nehmen alle 0 raus
# View(tbl.disease.gruppe1[,c("SIC","C_PUB_STAT_PUB_STATUS")]) ##bestimmte spalten aungucken
nrow(tbl.disease.gruppe1) ##2260

##Krankheiten, Medis ausschlieﬂen (PubStat ist schon aussortiert)
tbl.disease.gruppe1 <- tbl.disease.gruppe1[tbl.disease.gruppe1$C_DISEASE_TX_SD_ALLG == 0,] 
tbl.disease.gruppe1 <- tbl.disease.gruppe1[tbl.disease.gruppe1$C_DISEASE_TX_SD_HYPO == 0,] 
tbl.disease.gruppe1 <- tbl.disease.gruppe1[tbl.disease.gruppe1$C_DISEASE_TX_SD_HYPER == 0,]
tbl.disease.gruppe1 <- tbl.disease.gruppe1[tbl.disease.gruppe1$C_DISEASE_TX_FRUEHGEB == 0,]
tbl.disease.gruppe1 <- tbl.disease.gruppe1[tbl.disease.gruppe1$C_DISEASE_TX_ENDOKR == 0,]
tbl.disease.gruppe1 <- tbl.disease.gruppe1[tbl.disease.gruppe1$C_DISEASE_TX_DM1 == 0,]
tbl.disease.gruppe1 <- tbl.disease.gruppe1[tbl.disease.gruppe1$C_DISEASE_TX_DM2 == 0,]
tbl.disease.gruppe1 <- tbl.disease.gruppe1[tbl.disease.gruppe1$C_DISEASE_TX_NGSCREEN == 0,]
tbl.disease.gruppe1 <- tbl.disease.gruppe1[tbl.disease.gruppe1$C_DISEASE_TX_BLUT == 0,]
tbl.disease.gruppe1 <- tbl.disease.gruppe1[tbl.disease.gruppe1$C_DISEASE_TX_GERIN == 0,]
tbl.disease.gruppe1 <- tbl.disease.gruppe1[tbl.disease.gruppe1$C_DISEASE_TX_PULMO == 0,]
tbl.disease.gruppe1 <- tbl.disease.gruppe1[tbl.disease.gruppe1$C_DISEASE_TX_ASTHMA == 0,]
tbl.disease.gruppe1 <- tbl.disease.gruppe1[tbl.disease.gruppe1$C_DISEASE_TX_GIT == 0,]
tbl.disease.gruppe1 <- tbl.disease.gruppe1[tbl.disease.gruppe1$C_DISEASE_TX_INFEKT == 0,]
tbl.disease.gruppe1 <- tbl.disease.gruppe1[tbl.disease.gruppe1$C_DISEASE_TX_HWI_PN == 0,]
tbl.disease.gruppe1 <- tbl.disease.gruppe1[tbl.disease.gruppe1$C_DISEASE_TX_NIERENFEHL == 0,]
tbl.disease.gruppe1 <- tbl.disease.gruppe1[tbl.disease.gruppe1$C_DISEASE_TX_KARDIO_RHYTH == 0,]
tbl.disease.gruppe1 <- tbl.disease.gruppe1[tbl.disease.gruppe1$C_DISEASE_TX_KARDIO == 0,]
tbl.disease.gruppe1 <- tbl.disease.gruppe1[tbl.disease.gruppe1$C_DISEASE_TX_ALLERG == 0,]
tbl.disease.gruppe1 <- tbl.disease.gruppe1[tbl.disease.gruppe1$C_DISEASE_TX_EPIKRAMPF == 0,]
tbl.disease.gruppe1 <- tbl.disease.gruppe1[tbl.disease.gruppe1$C_DISEASE_TX_PSY == 0,]
tbl.disease.gruppe1 <- tbl.disease.gruppe1[tbl.disease.gruppe1$C_DISEASE_TX_ADHS == 0,]
tbl.disease.gruppe1 <- tbl.disease.gruppe1[tbl.disease.gruppe1$C_DISEASE_TX_DEPRES == 0,]
tbl.disease.gruppe1 <- tbl.disease.gruppe1[tbl.disease.gruppe1$C_DISEASE_TX_SUCHT == 0,]
tbl.disease.gruppe1 <- tbl.disease.gruppe1[tbl.disease.gruppe1$C_DISEASE_TX_MUSKEL == 0,]

nrow(tbl.disease.gruppe1)##1728, es gab also 532 Krankheitsausschl¸sse

# Medis ausschlieﬂen
tbl.disease.gruppe1 <- tbl.disease.gruppe1[tbl.disease.gruppe1$CHILD_MED_H_GLUCO_CORT == 0,]
tbl.disease.gruppe1 <- tbl.disease.gruppe1[tbl.disease.gruppe1$CHILD_MED_H_ASTHMA == 0,]
tbl.disease.gruppe1 <- tbl.disease.gruppe1[tbl.disease.gruppe1$CHILD_MED_H_WACHSTUM == 0,]
tbl.disease.gruppe1 <- tbl.disease.gruppe1[tbl.disease.gruppe1$CHILD_MED_H_IMMUNSUPP == 0,]
tbl.disease.gruppe1 <- tbl.disease.gruppe1[tbl.disease.gruppe1$CHILD_MED_H_NEUROLEPTIKA == 0,]
tbl.disease.gruppe1 <- tbl.disease.gruppe1[tbl.disease.gruppe1$CHILD_MED_H_ANTIPSYCH == 0,]
tbl.disease.gruppe1 <- tbl.disease.gruppe1[tbl.disease.gruppe1$CHILD_MED_H_ADHS == 0,]
tbl.disease.gruppe1 <- tbl.disease.gruppe1[tbl.disease.gruppe1$CHILD_MED_H_MINERALOCORT == 0,]
tbl.disease.gruppe1 <- tbl.disease.gruppe1[tbl.disease.gruppe1$CHILD_MED_H_HORMONE == 0,]
tbl.disease.gruppe1 <- tbl.disease.gruppe1[tbl.disease.gruppe1$CHILD_MED_H_METFORMIN == 0,]
tbl.disease.gruppe1 <- tbl.disease.gruppe1[tbl.disease.gruppe1$CHILD_MED_H_INSULIN == 0,]
tbl.disease.gruppe1 <- tbl.disease.gruppe1[tbl.disease.gruppe1$CHILD_MED_H_LTHYROX == 0,]
tbl.disease.gruppe1 <- tbl.disease.gruppe1[tbl.disease.gruppe1$CHILD_MED_H_KONTRAZEPT == 0,]
tbl.disease.gruppe1 <- tbl.disease.gruppe1[tbl.disease.gruppe1$CHILD_MED_H_SEX_STEROIDE == 0,]
tbl.disease.gruppe1 <- tbl.disease.gruppe1[tbl.disease.gruppe1$CHILD_MED_H_TESTO == 0,]
tbl.disease.gruppe1 <- tbl.disease.gruppe1[tbl.disease.gruppe1$CHILD_MED_H_ANTIBIO == 0,]
tbl.disease.gruppe1 <- tbl.disease.gruppe1[tbl.disease.gruppe1$CHILD_MED_H_ALLERGIE == 0,]
tbl.disease.gruppe1 <- tbl.disease.gruppe1[tbl.disease.gruppe1$CHILD_MED_H_ANTIHIST == 0,]
tbl.disease.gruppe1 <- tbl.disease.gruppe1[tbl.disease.gruppe1$CHILD_MED_H_HYPOSENS == 0,]

nrow(tbl.disease.gruppe1) #1487

## 3. Freitexte ausschlieﬂen:

##Als excel abspeichern in tabelle
write.xlsx(x =tbl.disease.gruppe1, file ="data/Gruppe1/tblGruppe1umFreitexteAuszusortieren.xlsx" ) 
t1 <- tbl.disease.gruppe1[ , c( "SIC", "SGROUP", "EDAT", 
               "CHILD_MED_H_MED_NAME", "CHILD_MED_H_ATC_NAME", "C_DISEASE_TX_FREITEXT_ANGABE") ] ## neue Tabelle t1: um nur bestimmt spalten anzugucken
# View( t1 )
write.xlsx(x = t1, file ="data/Gruppe1/c.xlsx" )
# Tabelle "Gruppe1um FreitexteAuszusortieren": Ausgeschlossen sind PubStat, KH und MEdis.
# Tabelle mit codierte Freitexten: t1FreitexteCodiert.xlsx --> laden+konvertieren und Freitexte ausschlieﬂen
#Freitexte in R ausschlieﬂen


t1FreitexteCodiert <- read_excel("C:/Users/Lea/Desktop/DA/LIFE-Daten/R-Projekte/r20170601/data/Gruppe1/t1FreitexteCodiert.xlsx")
# save("t1FreitexteCodiert", file = "t1FreitexteCodiert.Rd")
nrow(t1FreitexteCodiert) ##1487
# NAs der ganzen Spalte AKs auf 0 setzen
t1FreitexteCodiert$Aks[is.na(t1FreitexteCodiert$Aks)] <- 0

# # #Flags aussortieren: t2=tbl mit aussortierten freitexten und aussortierten spalten
# t2<- t1FreitexteCodiert <- t1FreitexteCodiert[t1FreitexteCodiert$Aks == 0,]
# nrow(t1FreitexteCodiert)#1425 (es gabe also 1487-1425=62 Freitext-Ausschl¸sse)-> m¸ssen noch zum Protokoll zugef¸gt werden

# #wieder in Excel tabelle ¸bertragen 
# write.xlsx(x = t1FreitexteCodiert,file = "Gruppe1FreitexteAusgeschlossen.xlsx")
#  getwd()

##codierte Freitexte zum Protokoll zuf¸gen --> mache ich manuell
##aus freitext krankheit erg√§nzen zum protokoll durch addition
# protokoll.Krankheit$Anzahl[ protokoll.Krankheit$Krankheit=="Schilddruese" ] <- 
# #   protokoll.Krankheit$Anzahl[ protokoll.Krankheit$Krankheit=="Schilddruese" ] +1

## Tabellen matchen: neue tabelle: t1FreitexteAlleFlags1 (kleine tbl.): laden und benennen, dann nur die Flags=1 behalten, dann...
t1FreitexteAlleFlags1 <- read_excel("C:/Users/Lea/Desktop/DA/LIFE-Daten/R-Projekte/r20170601/data/Gruppe1/t1FreitexteCodiertAlleFlagsAuf1.xlsx")
t1FreitexteAlleFlags1 <- t1FreitexteAlleFlags1[ !is.na( t1FreitexteAlleFlags1$Aks ) & t1FreitexteAlleFlags1$Aks == 1, ]
##...dann:Baue aus Kombination von SIC und SCIGROUP eine eindeutige Zeichenkette und fuege diesen in Spalte SS hinzu --> ich mache es mit SIC und Freitext..., da es SCIGroup nicht gibt in kleiner tbl.
t1FreitexteAlleFlags1$SS <- paste0( t1FreitexteAlleFlags1$SIC, t1FreitexteAlleFlags1$C_DISEASE_TX_FREITEXT_ANGABE )
# ...dann: Mache dasselbe mit Originaltabelle
tbl.disease.gruppe1$SS <- paste0(tbl.disease.gruppe1$SIC, tbl.disease.gruppe1$C_DISEASE_TX_FREITEXT_ANGABE )
#...dann:Entferne jetzt alle Zeilen, deren SIC-FREITEXT-Kombination in der der kleinen tbl. zu finden sind,
#   bzw behalte alle, die nicht dort zu finden sind
tbl.disease.gruppe1 <- tbl.disease.gruppe1[ !( tbl.disease.gruppe1$SS %in% t1FreitexteAlleFlags1$SS ), ]
#...dann: entferne SIC-FREITEXT-Spalte wieder
tbl.disease.gruppe1$SS <- NULL
nrow(tbl.disease.gruppe1) ##1432!:) also wurden 55 ausgeschlossen, da teils Doppelte SICs
#loesche kleine Tbl., da nicht mehr benoetigt
rm( list = c( "t1FreitexteAlleFlags1" ) )
#als neue Exccel-Tabelle speichern:
# write.xlsx(x = tbl.disease.gruppe1, file ="data/Gruppe1/gruppe1mitMehrfachbesuchen.xlsx" )


##Jetzt arbeite ich wieder mit tbl.disease.gruppe1.
## --> ausgeschlossen sind: PubStat, KH, Medis, Freitexte.


##deskription mit mehrfachbesuchern

 ##1. neue spalte"visit" erstellen, in der steht, ob es der Erst-, Zweit-, Dritt-Besuch ist
 ##2. Erstbesuche z‰hlen. Mehrfachbesuche: Tbl: Anzahl der besuche nach SEX
 ##3. Altersgruppen erstellen
 ##4. Anzahl der Besuche nach Geschlecht plotten
 ##5. Anzahl der Besuche nach PubStat/Sex
 ##6. tbl"visits": 

##t3=AKs Krankheiten, Medis, Pubstad, Freitexte
t3 <- tbl.disease.gruppe1[ !is.na( tbl.disease.gruppe1$EDAT ), ] ##fehlendende edats ausschlieﬂen
# 
t3 %<>%       ## pipe operator wird ausgef¸hrt und an t1 zur¸ckgeschickt
  group_by( SIC ) %>%  ##gruppen nach sics erstellt
  mutate( visit = dense_rank( EDAT ) ) ##mutate h‰ngt neue spalte ran, hier: visit: dem ersten visit wird =1 zugewisesn, dann 1en z‰hlen

##Erstbesuche Z‰hlen
sum( t3$visit == 1, na.rm = T )  #true=erstbesuch =1, false= alle anderen Besuche ##963 erstbesuche!
nrow(t3) ##insgesamt ohne rausschmiss 1432 besucher
table(t3$visit,t3$SEX) ##Mehrfachbesuche: tbl: zeigt anzahl nach gechlecht bei besuch: z.b.: bei erstbesuch waren 544female
                       ## bei Zweitbesuch 236 female ...-->insgesamt 1487 Mehrfachbesuche

##wieviele m‰nnliche/weibliche?
table(t3$SEX)

##plotte gesamtzahl der Besuche nach SEX
ggplot( t3 ) +
  geom_histogram( aes( visit ), stat = "count" ) +
  facet_grid( . ~ SEX ) +
  scale_fill_brewer( type = "qual", palette = 3 ) +
  xlab( "Gesamtzahl Besuche" ) +
  ylab( "Zahl der Probanden" ) +
  theme_bw( )


# ##Merhfachbesuch:erstelle Altersgruppen : neue spalte erstellen: age categorie
t3$AGE.CATEGORIE <-
  cut(x = t3$AGE,breaks = c(0:20), labels = c(1:20))
#View(t3[,c("SIC", "EDAT", "visit", "AGE", "AGE.CATEGORIE")])
# is.na(t1$C_PUB_STAT_PUB_STATUS)
#
# # Mehrfachb.: plotte Anzahlen der Besuche nach Alter/Geschlecht
  ggplot(t3) +
  my.theme +
  #scale_fill_manual(name = "Geschlecht", values = c("red", "blue")) +
  xlab("Altersklassen") +
  ylab("Anzahl der Besucher") +
  geom_histogram(aes(AGE.CATEGORIE, fill = as.factor( visit ) ), stat = "count", position = "stack") +
  facet_grid(. ~ SEX)
 #ggsave( filename = "BesuchAltersklassen.pdf" )
  
 
##Mehrfachb.: wieviele besuche pro pubstat
ggplot(t3) +
  my.theme +
  #scale_fill_manual(name = "Geschlecht", values = c("red", "blue")) +
  xlab("PubStat") +
  ylab("Anzahl der Besucher") +
  geom_histogram(aes(C_PUB_STAT_PUB_STATUS, fill = as.factor( visit ) ), stat = "count", position = "stack") +
  facet_grid(. ~ SEX) #. bedeutet: man kˆnnte noch andere variable nehmen
# ggsave( filename = "AnzahlBesuche~PubStat.pdf" )

fun <-
  function( v ) { return( v[ 1 ] ) }
##ermittle fuer jede SIC Anzahl der Besuche
tbl.visits <-
  tbl.disease.gruppe1 %>%
  group_by(SIC) %>% ##sortieren nach sic
  summarise(VISITS = as.factor(n()),  ##jede gruppe kriegt einen eintrag: visits=n z‰hlt gruppengrˆﬂe
            SEX = fun(SEX))
#neue Tabelle: visits: zeigt besuche nach SEX?????
(visits <-
    table(tbl.visits$SEX, tbl.visits$VISITS)) ##???tbl macht keinen Sinn: nur 963 erstbesucher, aberl 3 visit kategorien werden angezeigt

ggplot(tbl.visits) +
  geom_histogram(aes(VISITS, fill = SEX ), stat = "count" )+
  facet_grid(SEX~.)

ggplot(tbl.visits) +
  geom_histogram(aes(VISITS, fill = SEX ), stat = "count" )+
  facet_grid(.~SEX)

##Mehrfachb.: AnzahlBesucher~Alter mit Anteil des jeweiligen PubStat, nach SEX
ggplot(t3) +
  my.theme +
  #scale_fill_manual(name = "Geschlecht", values = c("red", "blue")) +
  xlab("Altersklassen") +
  ylab("Anzahl der Besucher") +
  geom_histogram(aes(AGE.CATEGORIE, fill = as.factor( C_PUB_STAT_PUB_STATUS ) ), stat = "count", position = "stack") +
  facet_grid(. ~ SEX)  ##¸bereinander: position="stack", nebeneinander w‰re: "dodge"
# ggsave( filename = "AnzahlBesuche~AlterMitPubStat.pdf" )

# ##mit neuer tabelle gruppe 1, visit= der 1-3. besuch  ??
# ggplot(tbl.disease.gruppe1) +
#   my.theme +
#   #scale_fill_manual(name = "Geschlecht", values = c("red", "blue")) +
#   xlab("Altersklassen") +
#   ylab("Anzahl der Besuche") +
#   geom_histogram(aes(AGE.CATEGORIE, fill = as.factor( C_PUB_STAT_PUB_STATUS ) ), stat = "count", position = "stack") +
#   facet_grid(. ~ SEX)  ##¸bereinander: position="stack", nebeneinander w‰re: "dodge"






##4. nur Erstbesucher nehmen: neue tbl: t4
t4<- t3[t3$visit==1,] #links vom Komma: zeilen, rechts splaten (wenn nur (,)= alle spalten nehmen
nrow(t4) #963 Erstbesuche
write.xlsx(x = t4, file ="data/Gruppe1/Gruppe1.fertigerDatensatz.xlsx" )

##--> Fertiger Datensatz Gruppe 1, n=963.


table(t4$SEX, t4$C_PUB_STAT_PUB_STATUS) ##tabelle um zu gucken wieviele in welchem pubstat

#wieviele male/female?
table(t4$SEX)

##Mittelwert etc des ALters
describeBy(t4$AGE, t4$SEX)

##describe Verschiedenes
describeBy(t4$LH_S_NUM_VALUE)
describeBy(t4$C_ANTHRO_KH_BMI_ORIG)

##Atersverteilung: 
table(t4$SEX)

maennlicheProbanden <- subset(t4, t4$SEX == "male")

hist(maennlicheProbanden$AGE, xlim = c(3, 20), ylim = c(0, 80), col = "blue", breaks = 20)

weiblicheProbanden <- subset(t4, t4$SEX == "female")
hist(weiblicheProbanden$AGE, xlim = c(3, 20), ylim = c(0, 80), col = "blue", breaks = 20)


##nach sex
ggplot(t4) +
  my.theme +
  #scale_fill_manual(name = "Geschlecht", values = c("red", "blue")) +
  xlab("PubStat") +
  ylab("Anzahl der Probanden") +
  geom_histogram(aes(x = as.factor( C_PUB_STAT_PUB_STATUS ) ), stat = "count", position = "stack") +
  facet_grid(. ~ SEX)  ##¸bereinander: position="stack", nebeneinander w‰re: "dodge", ##stat= count: z‰hlt anzhal pro pubstat
                      facet_grid(. ~ SEX) teilt Z‰hlung nach sex
# ggsave( filename = "AnzahlProbanden~PubStat.pdf" )

##ohne sex aufteilung
ggplot(t4) +
  my.theme +
  #scale_fill_manual(name = "Geschlecht", values = c("red", "blue")) +
  xlab("PubStat") +
  ylab("Anzahl der Probanden") +
  geom_histogram(aes(x = as.factor( C_PUB_STAT_PUB_STATUS ) ), stat = "count", position = "stack")
# ggsave( filename = "AnzahlBesuche~PubStatOhneSEX.pdf" )


##Scatterplots erstellen
# LH~alter  nach sex
ggplot(t4) +
  geom_point(aes(x = AGE, y = LH_S_NUM_VALUE, col=SEX))+                 #geom_point()= setzt punkte
 facet_grid(.~SEX)
# ggsave( filename = "LH~Alter_nachSEX.pdf" )

# oder beide sex zusammen:
ggplot(t4) +
  geom_point(aes(x = AGE, y = LH_S_NUM_VALUE, col=SEX))
# ggsave( filename = "LH~Alter_SexZusammen.pdf" )

##LH ~ Alter, nach Sex und PubStat
ggplot(t4) +
  geom_point(aes(x = AGE, y = LH_S_NUM_VALUE, col=SEX))+
  facet_grid(C_PUB_STAT_PUB_STATUS~SEX )

# LH~Alter nach SEX und PubStat mit limitierterer Y-Achse:
ggplot(t4) +
  geom_point(aes(x = AGE, y = LH_S_NUM_VALUE, col=SEX))+
  facet_grid(C_PUB_STAT_PUB_STATUS~SEX ) +
  ylim( c(0, 10))
# ggsave( filename = "LH~Alter_nachSEXundPubstat.pdf" )

##im selben Plot wie vorher die Adipˆsen-Falgs markieren
t4$C_DISEASE_TX_ADIP[ is.na( t4$C_DISEASE_TX_ADIP ) ] <- 2

ggplot(t4) +
  geom_point(aes(x = AGE, y = LH_S_NUM_VALUE, col=as.factor(C_DISEASE_TX_ADIP)))+ #as factor: da adipostitas flag 0 oder 1
  facet_grid(C_PUB_STAT_PUB_STATUS~SEX ) +
  scale_color_manual( name = "ADI", values = c( "red", "green", "blue") )+
ylim( c(0, 10))
# ggsave( filename = "LH~Alter_nachSEXundPubstatAdipˆseMarkiert.pdf" )


# # neue spalte f¸r kohorten zuweisung anlegen
t4$Kohorte <- NA
t4$Kohorte[grepl( "A", t4$SCIGROUP, perl = T) ] <- "A-Kohorte"
t4$Kohorte[grepl( "B", t4$SCIGROUP, perl = T) ] <- "B-Kohorte"

ggplot(t4) +
  geom_point(aes(x = AGE, y = LH_S_NUM_VALUE, col=Kohorte))+ #as factor: da adipostitas flag 0 oder 1
  facet_grid(C_PUB_STAT_PUB_STATUS~SEX )
ggsave( filename = "LH~Alter_nachSEXundPubstat.pdf" )

t4$Kohorte <- NULL ##spalte wieder lˆschen wenn ich sie nicht mehr brauche
 # unique( t4$SCIGROUP)



##Winkler
t4$D00177_SCORE_FAM <- cut(t4$D00177_SCORE_FAM, breaks = c(3, 8, 14, 21))
hist(t4$D00177_SCORE_FAM, breaks=0:22, col="darkseagreen3", main="")
table(t4$D00177_SCORE_FAM)

##Barplot in 3 Winklerkategorien
barplot(table(t4$D00177_SCORE_FAM))
# ggplot(t4) +
#   geom_point(aes(x = AGE, y = LH_S_NUM_VALUE, col=SEX))

#Plot: Anzahl der Probanden nach Winkler
ggplot(t4) +
  my.theme +
  xlab("Winkler-Kategorie") +
  ylab("Anzahl der Probanden") +
  geom_histogram(aes(x = as.factor( D00177_SCORE_FAM ) ), stat = "count", position = "stack")
ggsave( filename = "AnzahlBesuche~Winkler.pdf" )


##lineare Regression LH 
plot(t4$C_ANTHRO_KH_BMI_ORIG, t4$LH_S_NUM_VALUE, xlim = c(0, 50))
#Regressionsgerade
abline(lm(t4$C_ANTHRO_KH_BMI_ORIG~t4$LH_S_NUM_VALUE), col="red")

# ggplot:
ggplot (daten, aes (x = C_ANTHRO_KH_BMI_ORIG, y = LH_S_NUM_VALUE, colour = GESCHLECHT)) + geom_point() + geom_smooth(method=loess) + ylim(0, 10)

## Boxplot: mittlerer LH wert in Abhaengigkeit vom Geschlecht, 
# boxplot(t4$LH_S_NUM_VALUE ~ SEX, data = t4) #sagt kaum was aus


##S√§ulendiagramm LH wert in Abh. vom geschlecht
histogram( ~  LH_S_NUM_VALUE | SEX, data = t4)




