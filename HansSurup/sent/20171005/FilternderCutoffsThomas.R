# Laden aller nötigen Pakete

library(plyr)
library(hexbin)
library(psych)
library(ggplot2)
library(QuantPsyc) # f?r standardised coefficients
library(car)
library(lsr)
library(Rmisc)
library(corrplot)
library(ggplot2)
library(reshape)
library(lattice)
library(plyr)
library(ggsignif)
library(lavaan)
library(readxl)
library(rgl)
library(gclus)
library(data.table)
library(dplyr)
library(dtplyr)
library(lazyeval)
library(xlsx)
library(rJava)
library(WriteXLS)

rm( list = ls( ) ) 

# Leider kann ich irgendwie die breinigte Tabelle nicht speichern, deshabl muss ich jedes Mal die Ursprungsdatei reinigen
tbl.thyroid.complete <- read_excel("PV0365_Gesamt_Join.xlsx")

# erstmal Besuche mit fehlenden TSH-, fT3-, fT4- Werten ausschließen 

tbl.thyroid.complete <-
  tbl.thyroid.complete[!is.na(tbl.thyroid.complete$TSH_S_NUM_VALUE),] # 7705
tbl.thyroid.complete <-
  tbl.thyroid.complete[ !is.na( tbl.thyroid.complete$FT3_S_NUM_VALUE ), ] # nachgezählt 7477
tbl.thyroid.complete <-
  tbl.thyroid.complete[ !is.na( tbl.thyroid.complete$FT4_S_NUM_VALUE ), ] # 7406

# Ausschluss des fehlenden Geschlechts und Alters

tbl.thyroid.complete <-
  tbl.thyroid.complete[!is.na(tbl.thyroid.complete$TEILNEHMER_GESCHLECHT) & !is.na(tbl.thyroid.complete$C_ANTHRO_KH_AGE),]



##1.1 Alle NAs auf 0 setzen
tbl.thyroid.complete$C_DISEASE_TX_SD_ALLG[is.na(tbl.thyroid.complete$C_DISEASE_TX_SD_ALLG)] <- 0
tbl.thyroid.complete$C_DISEASE_TX_SD_HYPO[is.na(tbl.thyroid.complete$C_DISEASE_TX_SD_HYPO)] <- 0
tbl.thyroid.complete$C_DISEASE_TX_SD_HYPER[is.na(tbl.thyroid.complete$C_DISEASE_TX_SD_HYPER)] <- 0
tbl.thyroid.complete$C_DISEASE_TX_FRUEHGEB[is.na(tbl.thyroid.complete$C_DISEASE_TX_FRUEHGEB)] <- 0
tbl.thyroid.complete$C_DISEASE_TX_ENDOKR[is.na(tbl.thyroid.complete$C_DISEASE_TX_ENDOKR)] <- 0
tbl.thyroid.complete$C_DISEASE_TX_DM1[is.na(tbl.thyroid.complete$C_DISEASE_TX_DM1)] <- 0
tbl.thyroid.complete$C_DISEASE_TX_DM2[is.na(tbl.thyroid.complete$C_DISEASE_TX_DM2)] <- 0
tbl.thyroid.complete$C_DISEASE_TX_NGSCREEN[is.na(tbl.thyroid.complete$C_DISEASE_TX_NGSCREEN)] <- 0
tbl.thyroid.complete$C_DISEASE_TX_BLUT[is.na(tbl.thyroid.complete$C_DISEASE_TX_BLUT)] <- 0
tbl.thyroid.complete$C_DISEASE_TX_GERIN[is.na(tbl.thyroid.complete$C_DISEASE_TX_GERIN)] <- 0
tbl.thyroid.complete$C_DISEASE_TX_GIT[is.na(tbl.thyroid.complete$C_DISEASE_TX_GIT)] <- 0
tbl.thyroid.complete$C_DISEASE_TX_NIERENFEHL[is.na(tbl.thyroid.complete$C_DISEASE_TX_NIERENFEHL)] <- 0
tbl.thyroid.complete$C_DISEASE_TX_KARDIO_RHYTH[is.na(tbl.thyroid.complete$C_DISEASE_TX_KARDIO_RHYTH)] <- 0
tbl.thyroid.complete$C_DISEASE_TX_KARDIO[is.na(tbl.thyroid.complete$C_DISEASE_TX_KARDIO)] <- 0
tbl.thyroid.complete$C_DISEASE_TX_EPIKRAMPF[is.na(tbl.thyroid.complete$C_DISEASE_TX_EPIKRAMPF)] <- 0
tbl.thyroid.complete$C_DISEASE_TX_PSY[is.na(tbl.thyroid.complete$C_DISEASE_TX_PSY)] <- 0
tbl.thyroid.complete$C_DISEASE_TX_SUCHT[is.na(tbl.thyroid.complete$C_DISEASE_TX_SUCHT)] <- 0
tbl.thyroid.complete$C_DISEASE_TX_SUCHT[is.na(tbl.thyroid.complete$C_DISEASE_TX_NEPHRO)] <- 0




# 1.2 Flags zählen und protokollieren

sum (tbl.thyroid.complete$C_DISEASE_TX_SD_ALLG == 1) # 29
sum (tbl.thyroid.complete$C_DISEASE_TX_NEPHRO == 1)
sum (tbl.thyroid.complete$C_DISEASE_TX_SD_HYPER == 1)  # 5
sum (tbl.thyroid.complete$C_DISEASE_TX_SD_HYPO == 1)  # 36
sum (tbl.thyroid.complete$C_DISEASE_TX_ENDOKR == 1) #28
sum (tbl.thyroid.complete$C_DISEASE_TX_DM1 == 1) #5
sum (tbl.thyroid.complete$C_DISEASE_TX_DM2 == 1) # 13
sum (tbl.thyroid.complete$C_DISEASE_TX_BLUT == 1) #37
sum (tbl.thyroid.complete$C_DISEASE_TX_GIT == 1) #80
sum (tbl.thyroid.complete$C_DISEASE_TX_EPIKRAMPF == 1) #54
sum (tbl.thyroid.complete$C_DISEASE_TX_SUCHT == 1) #20


nrow(tbl.thyroid.complete)

# 1.3 Ausschluss der Flags 
tbl.thyroid.complete <- tbl.thyroid.complete[tbl.thyroid.complete$C_DISEASE_TX_SD_ALLG == 0,] 
tbl.thyroid.complete <- tbl.thyroid.complete[tbl.thyroid.complete$C_DISEASE_TX_SD_HYPO == 0,] #7341
tbl.thyroid.complete <- tbl.thyroid.complete[tbl.thyroid.complete$C_DISEASE_TX_SD_HYPER == 0,] #7377
tbl.thyroid.complete <- tbl.thyroid.complete[tbl.thyroid.complete$C_DISEASE_TX_ENDOKR == 0,] #7293
tbl.thyroid.complete <- tbl.thyroid.complete[tbl.thyroid.complete$C_DISEASE_TX_EPIKRAMPF == 0,] #6919


# 2.1 Medikamentenausschluss

# Protokoll

# 1. "CHILD_MED_H_JOD"   2. "CHILD_MED_H_STOFFWECHSEL"  3. "CHILD_MED_H_METFORMIN"
# 4. "CHILD_MED_H_INSULIN"   5. "CHILD_MED_H_ANTIHIST"  6. "CHILD_MED_H_HORMONE"
# 7. "CHILD_MED_H_L-THYROXIN"  8. "CHILD_MED_H_SEXUALSTEROIDE"  9. "CHILD_MED_H_WACHSTUM"
# 10. ""CHILD_MED_H_GLUCO"  11. "CHILD_MED_H_TESTO" 




# 2.2 Flags der Medikamente auf 0 setzen
tbl.thyroid.complete$CHILD_MED_H_JOD[is.na(tbl.thyroid.complete$CHILD_MED_H_JOD)] <- 0
tbl.thyroid.complete$CHILD_MED_H_HORMONE[is.na(tbl.thyroid.complete$CHILD_MED_H_HORMONE)] <- 0
tbl.thyroid.complete$CHILD_MED_H_STOFFWECHSEL[is.na(tbl.thyroid.complete$CHILD_MED_H_STOFFWECHSEL)] <- 0
tbl.thyroid.complete$CHILD_MED_H_METFORMIN[is.na(tbl.thyroid.complete$CHILD_MED_H_METFORMIN)] <- 0
tbl.thyroid.complete$CHILD_MED_H_LTHYROX[is.na(tbl.thyroid.complete$CHILD_MED_H_LTHYROX)] <- 0
tbl.thyroid.complete$CHILD_MED_H_SEX_STEROIDE[is.na(tbl.thyroid.complete$CHILD_MED_H_SEX_STEROIDE)] <- 0
tbl.thyroid.complete$CHILD_MED_H_WACHSTUM[is.na(tbl.thyroid.complete$CHILD_MED_H_WACHSTUM)] <- 0
tbl.thyroid.complete$CHILD_MED_H_GLUCO_CORT[is.na(tbl.thyroid.complete$CHILD_MED_H_GLUCO_CORT)] <- 0
tbl.thyroid.complete$CHILD_MED_H_TESTO[is.na(tbl.thyroid.complete$CHILD_MED_H_TESTO)] <- 0
tbl.thyroid.complete$CHILD_MED_H_KONTRAZEPT[is.na(tbl.thyroid.complete$CHILD_MED_H_KONTRAZEPT)] <- 0
tbl.thyroid.complete$CHILD_MED_H_VITAMIN_D[is.na(tbl.thyroid.complete$CHILD_MED_H_VITAMIN_D)] <- 0




# 2.3 Flags (1) zählen und protokollieren

sum (tbl.thyroid.complete$CHILD_MED_H_JOD == 1)  #7
sum (tbl.thyroid.complete$CHILD_MED_H_HORMONE == 1) #193
sum (tbl.thyroid.complete$CHILD_MED_H_STOFFWECHSEL == 1) #28
sum (tbl.thyroid.complete$CHILD_MED_H_METFORMIN == 1) #10
sum (tbl.thyroid.complete$CHILD_MED_H_INSULIN == 1) #3
sum (tbl.thyroid.complete$CHILD_MED_H_ANTIHIST == 1) #146
sum (tbl.thyroid.complete$CHILD_MED_H_LTHYROX == 1) #46
sum (tbl.thyroid.complete$CHILD_MED_H_SEX_STEROIDE == 1) #117
sum (tbl.thyroid.complete$CHILD_MED_H_WACHSTUM == 1) #10
sum (tbl.thyroid.complete$CHILD_MED_H_GLUCO_CORT == 1) #241
sum (tbl.thyroid.complete$CHILD_MED_H_TESTO == 1) #2
sum (tbl.thyroid.complete$CHILD_MED_H_ASTHMA == 1) #36
sum (tbl.thyroid.complete$CHILD_MED_H_KONTRAZEPT == 1) 
sum (tbl.thyroid.complete$CHILD_MED_H_VITAMIN_D == 1) 






#2.4 Flags (1) ausschließen

tbl.thyroid.complete <- tbl.thyroid.complete[tbl.thyroid.complete$CHILD_MED_H_JOD == 0,] #6551
tbl.thyroid.complete <- tbl.thyroid.complete[tbl.thyroid.complete$CHILD_MED_H_HORMONE == 0,] #6444
tbl.thyroid.complete <- tbl.thyroid.complete[tbl.thyroid.complete$CHILD_MED_H_METFORMIN == 0,] #6435
tbl.thyroid.complete <- tbl.thyroid.complete[tbl.thyroid.complete$CHILD_MED_H_LTHYROX == 0,] #6285
tbl.thyroid.complete <- tbl.thyroid.complete[tbl.thyroid.complete$CHILD_MED_H_SEX_STEROIDE == 0,] #6212
tbl.thyroid.complete <- tbl.thyroid.complete[tbl.thyroid.complete$CHILD_MED_H_WACHSTUM == 0,] #6203
tbl.thyroid.complete <- tbl.thyroid.complete[tbl.thyroid.complete$CHILD_MED_H_GLUCO_CORT == 0,] #6153
tbl.thyroid.complete <- tbl.thyroid.complete[tbl.thyroid.complete$CHILD_MED_H_TESTO == 0,]
tbl.thyroid.complete <- tbl.thyroid.complete[tbl.thyroid.complete$CHILD_MED_H_KONTRAZEPT == 0,]



# Einzelfälle ausschließen

# Tabelle mit Ausschlüssen einspeisen
ausschluss2508 <- read_excel("AusschlussFreitextDiseases230817.xlsx")

# Filtern
ausschluss2508[!is.na(ausschluss2508$`Flag Ausschluss`) & ausschluss2508$`Flag Ausschluss`==1,] -> kandidaten

# Suchen der SICs
at <- tbl.thyroid.complete
at$SEX <- as.factor( c( "male", "female" )[ match( at$TEILNEHMER_GESCHLECHT,c(1,2))])
at <- at[!paste0( at$TEILNEHMER_SIC, at$C_ANTHRO_KH_GRP )  %in% paste0( kandidaten$TEILNEHMER_SIC, kandidaten$C_ANTHRO_KH_GRP),]
tbl.thyroid.complete2508 <- at

# Schreiben der Tabelle
xlsx::write.xlsx(x = tbl.thyroid.complete2508, file = "GesamtJoin_Bereinigt2908.xlsx")

# Leider funktioniert das Schreiben der Tabelle nicht, folgende Fehlermeldung ploppt auf:
# Fehler in .jcheck(silent = FALSE) : 
# Java Exception <no description because toString() failed>.jcall(row[[ir]], "Lorg/apache/poi/ss/usermodel/Cell;", "createCell", as.integer(colIndex[ic] - 1))<S4 object of class "jobjRef">

# Nach Reinigung der Daten nun zum Untersuchen der CutOffs

# Visualisierung der TSH- Werte

ggplot( tbl.thyroid.complete2508 ) +
  geom_point( 
    aes( C_ANTHRO_KH_AGE, at$TSH_S_NUM_VALUE ), alpha = .2 ) +
  facet_grid( SEX ~ . ) +
  theme_bw( ) +
  xlab( "Alter der Probanden" ) +
  ylab( "TSH- Serumkonzentration" )
 

# Betrachtung der unteren und oberen Grenze 2,5 %
quantile(at$TSH_S_NUM_VALUE, c(.025, .975))

# Kontrolle der unteren 2,5 % auf eventuelle Krankheiten oder Unstimmigkeiten in Größe/ Gewicht

View(at[at$TSH_S_NUM_VALUE <= 0.992225,c("TEILNEHMER_GESCHLECHT", "TEILNEHMER_SIC", "C_DISEASE_TX_FREITEXT_ANGABE", "C_ANTHRO_KH_AGE", "C_ANTHRO_KH_HEIGHT_ORIG", "C_ANTHRO_KH_HEIGHT_ADJ", "C_ANTHRO_KH_WEIGHT_ORIG", "C_ANTHRO_KH_WEIGHT_ADJ", "TSH_S_NUM_VALUE" )])

cutofflowtsh <- at[at$TSH_S_NUM_VALUE <= 0.992225,c("TEILNEHMER_GESCHLECHT", "TEILNEHMER_SIC", "C_DISEASE_TX_FREITEXT_ANGABE", "C_ANTHRO_KH_AGE", "C_ANTHRO_KH_HEIGHT_ORIG", "C_ANTHRO_KH_HEIGHT_ADJ", "C_ANTHRO_KH_WEIGHT_ORIG", "C_ANTHRO_KH_WEIGHT_ADJ", "TSH_S_NUM_VALUE" )]

WriteXLS("cutofflowtsh",ExcelFileName="cutofflowtsh.xlsx",row.names=F,col.names=T)


# Kontrolle der oberen 2,5 % auf eventuelle Krankheiten oder Unstimmigkeiten in Größe/ Gewicht

View(at[at$TSH_S_NUM_VALUE >= 5.870000,c("TEILNEHMER_GESCHLECHT", "TEILNEHMER_SIC", "C_DISEASE_TX_FREITEXT_ANGABE", "C_ANTHRO_KH_AGE", "C_ANTHRO_KH_HEIGHT_ORIG", "C_ANTHRO_KH_HEIGHT_ADJ", "C_ANTHRO_KH_WEIGHT_ORIG", "C_ANTHRO_KH_WEIGHT_ADJ", "TSH_S_NUM_VALUE" )])

cutoffhightsh <- at[at$TSH_S_NUM_VALUE >= 5.870000,c("TEILNEHMER_GESCHLECHT", "TEILNEHMER_SIC", "C_DISEASE_TX_FREITEXT_ANGABE", "C_ANTHRO_KH_AGE", "C_ANTHRO_KH_HEIGHT_ORIG", "C_ANTHRO_KH_HEIGHT_ADJ", "C_ANTHRO_KH_WEIGHT_ORIG", "C_ANTHRO_KH_WEIGHT_ADJ", "TSH_S_NUM_VALUE" )]

WriteXLS("cutoffhightsh",ExcelFileName="cutoffhightsh.xlsx",row.names=F,col.names=T)

# Frage: Im ggplot tauchen Ausreißer auf, die in der Tabelle nicht zu finden sind, woran liegt das? Könnten die Altersangaben in der erstellten Tabelle verrutscht sein?
#        Wie schreibe ich eine Tabelle in der gleichzeitig die Quantile 0 - 2,5% und 97,5 - 100% stecken?
#        Warum kann ich die Tabelle Gesamt_Join nicht umschreiben?
# Vielen Dank!


