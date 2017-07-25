setwd("C:/Users/Lea/Desktop/DA/LIFE-Daten")
library("readxl") ## excel laden
library(dplyr) ## data wrangling
library(lubridate) ## paket für zeit und datumsmanipulationen

## stammdaten
persdat <- read_excel("data/PV0298_R00001.xlsx")

## namen kuerzen
names(persdat) <- gsub("TEILNEHMER_","",names(persdat))

### nur die spalten, die wir brauchen
persdat <- select(persdat, SIC, GESCHLECHT, GEB_JJJJMM)

## geburtsdatum erzeugen
persdat$gebdat <- ymd(paste0(persdat$GEB_JJJJMM,15))

## alte geburtsmonats spalte loeschen
persdat$GEB_JJJJMM <- NULL

##für geschlecht male/female schreiben anstatt 1/2
persdat$sex <- factor(persdat$GESCHLECHT,
                      levels = c(1,2),
                      labels = c("male","female"))

## anthro laden
anthro <- read_excel("data/PV0298_D00040_NODUP.xlsx") ## gr, gew, bmi mit SDS nach KH

##Anthrospalte loeschen, die ich nicht brauche
anthro$C_ANTHRO_KH_UID <- NULL
  
## persdat + anthro mergen
daten <- merge(persdat, anthro,
               by.x = "SIC",
               by.y = "C_ANTHRO_KH_SIC")

##FSH laden  
FSH <- read_excel("data/PV0298_T00487_NODUP.xlsx")

##FSH Spalten auswählen
FSH <- select(FSH, FSH_S_SIC, FSH_S_DATUM, FSH_S_GRUPPE, 
              FSH_S_NUM_VALUE, FSH_S_VALUE_FLAG,
              FSH_S_REF_RANGE_L, FSH_S_REF_RANGE_H)

##FSH mergen mit daten
daten <- merge (daten, FSH, 
                by.x = c("SIC", "C_ANTHRO_KH_GRP"), 
                by.y = c ("FSH_S_SIC", "FSH_S_GRUPPE"),
                all.x = TRUE) ##alle daten aus 1. datensatz behalten, auch wenn nicht im 2. vorhanden
##FSH löschen
FSH <- NULL

## Fehlwerte zaehlen
colSums(is.na(daten))

range(daten$C_ANTHRO_KH_AGE)
table(daten$C_ANTHRO_KH_GRP)
## 

## Boxplot vom FSH wert in Abhaengigkeit vom Geschlecht, 
## Variablen aus dem datensatz daten
boxplot(FSH_S_NUM_VALUE ~ sex, data = daten)

##Säulendiagramm FSH wert in Abh. vom geschlecht
library(lattice)
histogram( ~  FSH_S_NUM_VALUE | sex, data = daten)


LH <- read_excel("data/PV0298_T00508_NODUP.xlsx")
##LH Spalten auswählen
LH <- select(LH, LH_S_SIC, LH_S_DATUM, LH_S_GRUPPE, 
              LH_S_NUM_VALUE, LH_S_VALUE_FLAG,
              LH_S_REF_RANGE_L, LH_S_REF_RANGE_H)

##LH mergen mit daten
daten <- merge (daten, LH, 
                by.x = c("SIC", "C_ANTHRO_KH_GRP"), 
                by.y = c ("LH_S_SIC", "LH_S_GRUPPE"),
                all.x = TRUE) ##alle daten aus 1. datensatz behalten, auch wenn nicht im 2. vorhanden

##LH löschen
LH <- NULL

## Boxplot vom LH wert in Abhaengigkeit vom Geschlecht, 
## Variablen aus dem datensatz daten
boxplot(LH_S_NUM_VALUE ~ sex, data = daten)

## Säulendiagramm LH wert in Abh. vom Geschlecht
library(lattice)
histogram( ~  LH_S_NUM_VALUE | sex, data = daten)



Winkler <- read_excel("data/PV0298_D00128_NODUP.xlsx")
PG <- read_excel("data/PV0298_D00077_NODUP.xlsx")
Estradiol1 <- read_excel("data/PV0298_T00488_NODUP.xlsx")
Estradiol2 <- read_excel("data/PV0298_T00894_NODUP.xlsx")
SHBG <- read_excel("data/PV0298_T00489_NODUP.xlsx")
DHEAS <- read_excel("data/PV0298_T00490_NODUP.xlsx")
Testos <- read_excel("data/PV0298_T00509_NODUP.xlsx")
Medis <- read_excel("data/PV0298_D00129_NODUP.xlsx")
KH <- read_excel("data/PV0298_D00127_NODUP.xlsx")

summary(x$C_ANTHRO_KH_SIC)
summary(x$C_ANTHRO_KH_HEIGHT_ADJ)
