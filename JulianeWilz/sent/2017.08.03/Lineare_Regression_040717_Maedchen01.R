
# Loesche Speicher
rm( list =  ls( ) )

# Lade benoetigte Pakete
library( readxl )
library( dplyr )
library( ggplot2 )
library( GGally )
library( Hmisc )
library(knitr)
library (knitLatex)
library (rmarkdown)
library( reshape2 )
library( effects )


setwd( "~/Desktop/pv0116_neu/")

daten <-
  read_excel( "Tabellen/AktuelleTabelle190517excel.xlsx" )


d <-
  daten[ !is.na( daten$AGE_Calcitionin ) & !is.na( daten$CT_S_1_NUM_VALUE ), ]
d$AGE_Calcitionin <- as.numeric( d$AGE_Calcitionin )
d.m <- d[ d$TEILNEHMER_GESCHLECHT=="male", ]
d.f <- d[ d$TEILNEHMER_GESCHLECHT=="female", ]

# ich betrachte Jungen und Mädchen getrennt
# ich betrachte bei beiden nur das erste Lebensjahr
d01 <- d[between(d$AGE_Calcitionin, 0,1),]

d01m <- d.m[between(d.m$AGE_Calcitionin, 0,1),]
d01f <- d.f[between(d.f$AGE_Calcitionin, 0,1),]

# dann habe ich eine lineare Regression durchgeführt und mir den Zusammenhang
# von Calcitonin und dem Alter angesehen
# ich betrachte nur die Mädchen
d <-
  daten[ !is.na( daten$AGE_Calcitionin ) & !is.na( daten$CT_S_1_NUM_VALUE ), ]


d$AGE_Calcitionin <- as.numeric( d$AGE_Calcitionin )

d.m <- d[ d$TEILNEHMER_GESCHLECHT=="male", ]
d.f <- d[ d$TEILNEHMER_GESCHLECHT=="female", ]

d01 <- d[between(d$AGE_Calcitionin, 0,1),]

d01m <- d.m[between(d.m$AGE_Calcitionin, 0,1),]
d01f <- d.f[between(d.f$AGE_Calcitionin, 0,1),]


linreg <- lm(d01f$CT_S_1_NUM_VALUE ~ d01f$AGE_Calcitionin)
summary(linreg)

#Alter ist signifikant

#P1NP

#lineare Regression von Calcitonin mit dem Alter und P1NP
linreg <- lm(d01f$CT_S_1_NUM_VALUE ~ d01f$AGE_Calcitionin + d01f$P1NP_S_NUM_VALUE)
summary(linreg)
#P1NP und Alter sind nicht signifikant

#P1NP alleine
linreg <- lm(d01f$CT_S_1_NUM_VALUE ~  d01f$P1NP_S_NUM_VALUE)
summary(linreg)
#P1NP alleine ist signifikant

#lineare Regression von Calcitonin mit dem Alter und P1NP und Osteocalcin
linreg <- lm(d01f$CT_S_1_NUM_VALUE ~ d01f$AGE_Calcitionin +
               d01f$P1NP_S_NUM_VALUE + d01f$OSTEO_S_NUM_VALUE)
summary(linreg)
# keine Signifikanz 

#Osteocalcin
#lineare Regression von Calcitonin mit dem Alter und Osteocalcin
linreg <- lm(d01f$CT_S_1_NUM_VALUE ~ d01f$AGE_Calcitionin +
               d01f$OSTEO_S_NUM_VALUE)
summary(linreg)
##Osteocalcin ist nicht signifikant

#Osteocalcin
linreg <- lm(d01f$CT_S_1_NUM_VALUE ~ 
               d01f$OSTEO_S_NUM_VALUE)
summary(linreg)
#Osteocalcin alleine ist signifikant


#PTH

linreg <- lm(d01f$CT_S_1_NUM_VALUE ~ d01f$AGE_Calcitionin + d01f$PTH_S_NUM_VALUE)
summary(linreg)
#PTH ist nicht signifikant

linreg <- lm(d01f$CT_S_1_NUM_VALUE ~ d01f$PTH_S_NUM_VALUE)
summary(linreg)
#PTH alleine ist signifikant

#PTH ohne Ausreißer
d01f.p.aus <- d01f[ d01f$PTH_S_NUM_VALUE != 20.73, ]
linreg <- lm(d01f.p.aus$CT_S_1_NUM_VALUE ~ d01f.p.aus$AGE_Calcitionin + d01f.p.aus$PTH_S_NUM_VALUE)
summary(linreg)

#Phosphat

#lineare Regression von Calcitonin mit dem Alter und Phosphat
linreg <- lm(d01f$CT_S_1_NUM_VALUE ~ d01f$AGE_Calcitionin + d01f$P_S_NUM_VALUE)

summary(linreg)

linreg <- lm(d01f$CT_S_1_NUM_VALUE ~ d01f$P_S_NUM_VALUE)

summary(linreg)
#Phosphat ist nicht signifikant, wenn das Alter eingerechnet wird



linreg <- lm(d01f$CT_S_1_NUM_VALUE ~ d01f$AGE_Calcitionin +
               d01f$P1NP_S_NUM_VALUE + d01f$P_S_NUM_VALUE)
summary(linreg)
#auch nicht, wenn P1NP dabei ist


# die folgenden Parameter wiesen keine Korrelation auf, wurden also nur aus Interesse in die 
# lineare Regression miteinbezogen
#Calcium

#lineare Regression von Calcitonin mit dem Alter und Calcium
linreg <- lm(d01f$CT_S_1_NUM_VALUE ~ d01f$AGE_Calcitionin + d01f$CA_S_NUM_VALUE)

summary(linreg)
#Calcium ist nicht signifikant
linreg <- lm(d01f$CT_S_1_NUM_VALUE ~  d01f$CA_S_NUM_VALUE)

summary(linreg)
#auch alleine nicht

#Gewicht_SDS

#lineare Regression von Calcitonin mit dem Alter und Gewicht_SDS
linreg <- lm(d01f$CT_S_1_NUM_VALUE ~ d01f$AGE_Calcitionin + d01f$C_ANTHRO_KH_WEIGHT_ADJ)

summary(linreg)

#Gewicht SDS nicht signifikant

#BMI_SDS

#lineare Regression von Calcitonin mit dem Alter und BMI_SDS
linreg <- lm(d01f$CT_S_1_NUM_VALUE ~ d01f$AGE_Calcitionin + d01f$C_ANTHRO_KH_BMI_ADJ)

summary(linreg)
#BMI_SDS ist nicht signifikant

linreg <- lm(d01f$CT_S_1_NUM_VALUE ~ d01f$AGE_Calcitionin * d01f$C_ANTHRO_KH_BMI_ADJ)

summary(linreg)

#auch zusammen nicht signifikant

#lineare Regression von Calcitonin mit dem Alter und P1NP und BMI_SDS
linreg <- lm(d01f$CT_S_1_NUM_VALUE ~ d01f$AGE_Calcitionin + d01f$P1NP_S_NUM_VALUE + d01f$C_ANTHRO_KH_BMI_ADJ)

summary(linreg)

#Zusammenhang von Calcitonin mit Alter, P1NP und BMI_SDS ist nicht signifikant

#lineare Regression von Calcitonin mit dem Alter und Osteocalcin und BMI_SDS
linreg <- lm(d01f$CT_S_1_NUM_VALUE ~ d01f$AGE_Calcitionin + d01f$OSTEO_S_NUM_VALUE + d01f$C_ANTHRO_KH_BMI_ADJ)
summary (linreg)
#Zusammenhang von Calcitonin mit Alter, Osteocalcin und BMI_SDS ist nicht signifikant

#IGF-1

#lineare Regression von Calcitonin mit dem Alter und IGF-1
linreg <- lm(d01f$CT_S_1_NUM_VALUE ~ d01f$AGE_Calcitionin + d01f$IGF1_S_2_NUM_VALUE)

summary(linreg)

#IGF1 nicht signifikant


#Fazit: es besteht nur ein Zusammenhang zwischen Calcitonin und dem Alter bzw. jeder weiterer
#Zusammenhang besteht nicht mehr, sobald das Alter einbezogen wird


#Alter, BMI, Calcium

linreg <- lm(d01f$CT_S_1_NUM_VALUE ~ d01f$AGE_Calcitionin + d01f$C_ANTHRO_KH_BMI_ORIG  + d01f$CA_S_NUM_VALUE )
summary(linreg)
#nur Alter signifikant, der Rest nicht

#Alter, BMI-SDS, Calcium
linreg <- lm(d01f$CT_S_1_NUM_VALUE ~ d01f$AGE_Calcitionin + d01f$C_ANTHRO_KH_BMI_ADJ  + d01f$CA_S_NUM_VALUE )
summary(linreg)
#nur Alter signifikant


#Alter, Calcium, IGF-1

linreg <- lm(d01f$CT_S_1_NUM_VALUE ~ d01f$AGE_Calcitionin + d01f$IGF1_S_2_NUM_VALUE  + d01f$CA_S_NUM_VALUE )
summary(linreg)
# nur Alter signifikant

#Alter,Calcium, IGF-1-SDS

linreg <- lm(d01f$CT_S_1_NUM_VALUE ~ d01f$AGE_Calcitionin + d01f$ZIGF_S_2_NUM_VALUE  + d01f$CA_S_NUM_VALUE )
summary(linreg)
#nur Alter signifikant

#Alter,Calcium, Phosphat

linreg <- lm(d01f$CT_S_1_NUM_VALUE ~ d01f$AGE_Calcitionin + d01f$P_S_NUM_VALUE  + d01f$CA_S_NUM_VALUE )
summary(linreg)
#nur Alter signifikant

#Alter, P1NP, Calcium

linreg <- lm(d01f$CT_S_1_NUM_VALUE ~ d01f$AGE_Calcitionin + d01f$P1NP_S_NUM_VALUE  + d01f$CA_S_NUM_VALUE )
summary(linreg)
#signifikant

#Alter, Calcium, IGF-1-SDS, P1NP

linreg <- lm(d01f$CT_S_1_NUM_VALUE ~ d01f$AGE_Calcitionin + d01f$ZIGF_S_2_NUM_VALUE  + d01f$CA_S_NUM_VALUE + d01f$P1NP_S_NUM_VALUE)
summary(linreg)
# nur Alter signifikant

#Alter, Calcium, IGF-1, P1NP

linreg <- lm(d01f$CT_S_1_NUM_VALUE ~ d01f$AGE_Calcitionin + d01f$IGF1_S_2_NUM_VALUE  + d01f$CA_S_NUM_VALUE + d01f$P1NP_S_NUM_VALUE)
summary(linreg)
#Ca und P1NP signifikant, Alter und IGF-1 deutlich nicht



#Alter, P1NP, Calcium, Osteocalcin

linreg <- lm(d01f$CT_S_1_NUM_VALUE ~ d01f$AGE_Calcitionin + d01f$P1NP_S_NUM_VALUE  + d01f$CA_S_NUM_VALUE + d01f$OSTEO_S_NUM_VALUE)
summary(linreg)
#alle bis auf Osteocalcin sind signifikant


#P1NP, Ca
linreg <- lm(d01f$CT_S_1_NUM_VALUE ~  d01f$P1NP_S_NUM_VALUE  + d01f$CA_S_NUM_VALUE)
summary(linreg)
#nur P1NP signifikant

#P1NP, Osteocalcin
linreg <- lm(d01f$CT_S_1_NUM_VALUE ~  d01f$P1NP_S_NUM_VALUE  + d01f$OSTEO_S_NUM_VALUE)
summary(linreg)
#P1NP signifikant, Osteocalcin knapp nicht

#P1NP und IGF-1
linreg <- lm(d01f$CT_S_1_NUM_VALUE ~  d01f$P1NP_S_NUM_VALUE  + d01f$IGF1_S_2_NUM_VALUE)
summary(linreg)
#nur P1NP signifikant

#P1NP und IGF-1-SDS
linreg <- lm(d01f$CT_S_1_NUM_VALUE ~  d01f$P1NP_S_NUM_VALUE  + d01f$ZIGF_S_2_NUM_VALUE)
summary(linreg)
#nur P1NP signifkant

#Alter und Größe
linreg <- lm(d01f$CT_S_1_NUM_VALUE ~  d01f$AGE_Calcitionin  + d01f$C_ANTHRO_KH_HEIGHT_ORIG)
summary(linreg)

#Alter und Größe-SDS
linreg <- lm(d01f$CT_S_1_NUM_VALUE ~  d01f$AGE_Calcitionin  + d01f$C_ANTHRO_KH_HEIGHT_ADJ)
summary(linreg)

#n-Zahlen

rm( list =  ls( ) )
setwd( "~/Desktop/pv0116_neu/")

daten <-
  read_excel( "Tabellen/AktuelleTabelle190517excel.xlsx" )

d <-
  daten[ !is.na( daten$AGE_Calcitionin ) & !is.na( daten$CT_S_1_NUM_VALUE )& !is.na( daten$P1NP_S_NUM_VALUE )& !is.na( daten$OSTEO_S_NUM_VALUE ), ]

d$AGE_Calcitionin <- as.numeric( d$AGE_Calcitionin )


d.f <- d[ d$TEILNEHMER_GESCHLECHT=="female", ]

d01f <- d.f[between(d.f$AGE_Calcitionin, 0,1),]
