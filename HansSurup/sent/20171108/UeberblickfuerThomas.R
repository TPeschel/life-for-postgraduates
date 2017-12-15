# Freigeben des Speichers
rm( list = ls( ) ) 

# Installiere gegebenenfalls und lade noetige Pakete

hlpr4life::load.pkgs(
  c( 
      "hlpr4life",
      "ggplot2",
      "readxl",
      "openxlsx",
      "xlsx",
      "dplyr" ) )


# Hier musst Du wieder Deinen Pfad anpassen
setwd( "~/LIFE/life-for-postgraduates/HansSurup/sent/20171108/" )
#setwd( "~/Documents/Schule und Uni/Uni/Doktorarbeit/DieArbeitII/LIfE/Eigen Erstelltes/" )

tbl.thyroid.complete <-
    read_excel("PV0365_Gesamt_Join.xlsx" )

# Baue als erstes SEX-Spalte mit Woertern statt Zahlen
tbl.thyroid.complete$SEX <-
    as.factor( c( "male", "female" )[ match( tbl.thyroid.complete$TEILNEHMER_GESCHLECHT, c( 1, 2 ) ) ] )

# erstmal Besuche mit fehlenden TSH-, fT3-, fT4- Werten ausschließen 
tbl.thyroid.complete <-
  tbl.thyroid.complete[ !is.na( tbl.thyroid.complete$TSH_S_NUM_VALUE ), ] # 7705

tbl.thyroid.complete <-
  tbl.thyroid.complete[ !is.na( tbl.thyroid.complete$FT3_S_NUM_VALUE ), ] # nachgezählt 7477

tbl.thyroid.complete <-
  tbl.thyroid.complete[ !is.na( tbl.thyroid.complete$FT4_S_NUM_VALUE ), ] # 7406

# Ausschluss des fehlenden Geschlechts und Alters

tbl.thyroid.complete <-
  tbl.thyroid.complete[ !is.na( tbl.thyroid.complete$TEILNEHMER_GESCHLECHT ) & !is.na( tbl.thyroid.complete$C_ANTHRO_KH_AGE ), ] #7378

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

sum( tbl.thyroid.complete$C_DISEASE_TX_SD_ALLG == 1 ) # 29
sum( tbl.thyroid.complete$C_DISEASE_TX_SD_HYPER == 1 )  # 5
sum( tbl.thyroid.complete$C_DISEASE_TX_SD_HYPO == 1 )  # 36
sum( tbl.thyroid.complete$C_DISEASE_TX_ENDOKR == 1 ) #28
sum( tbl.thyroid.complete$C_DISEASE_TX_EPIKRAMPF == 1 ) #54
sum( tbl.thyroid.complete$C_DISEASE_TX_PSY == 1 ) #20 zur Betrachtung im Freitext

nrow( tbl.thyroid.complete )

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
sum (tbl.thyroid.complete$CHILD_MED_H_HORMONE == 1) #220
sum (tbl.thyroid.complete$CHILD_MED_H_METFORMIN == 1) #10
sum (tbl.thyroid.complete$CHILD_MED_H_LTHYROX == 1) #49
sum (tbl.thyroid.complete$CHILD_MED_H_SEX_STEROIDE == 1) #134
sum (tbl.thyroid.complete$CHILD_MED_H_WACHSTUM == 1) #11
sum (tbl.thyroid.complete$CHILD_MED_H_GLUCO_CORT == 1) #271
sum (tbl.thyroid.complete$CHILD_MED_H_TESTO == 1) #2
sum (tbl.thyroid.complete$CHILD_MED_H_KONTRAZEPT == 1) #101







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

# Habe leider nur eine alte Ausschlusstabelle
# Diese enthaelt keine Spalte "Flag Ausschluesse"
# Hier heisst die Spalte nur "Flag"
# Muesstest Du anpassen

##
# Benutze NIEMALS NIEMALS NIEMALS Leerzeichen ü,ö,ä,ß oder Sonderzeichen in Spaltennamen!!!!!!!
##
# Musst Du dann Deine wieder einsetzen
ausschluss2508 <-
    read_excel( "AusschlussFreitextDiseases1.xlsx" )

# Tabelle mit Ausschlüssen einspeisen
#ausschluss2508 <- read_excel("AusschlussFreitextDiseases230817.xlsx")

# Filtern
# Hole die Sics, bei denen eine 1 im Flag steht und speichere sie in kandidaten.sics
( kandidaten.sics <-
    ausschluss2508$TEILNEHMER_SIC[ !is.na( ausschluss2508$Flag ) & ausschluss2508$Flag == 1 ] )

# Behalte alle Zeilen aus Tabelle, deren Sic nicht in der kandidaten.sic-Liste sind
tbl.thyroid.complete. <-
    tbl.thyroid.complete[ !tbl.thyroid.complete$TEILNEHMER_SIC %in% kandidaten.sics, ]
