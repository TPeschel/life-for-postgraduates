rm( list = ls())

library( "readxl" )
library("WriteXLS")
library("xlsx")
library("dplyr")
library("ggplot2")

## Setze hier den Pfad zum Verzeichnis Deiner Dateien
setwd( "C:/Users/Anwender/Documents/Dissertation/PV208/data/data_neu_20160929" )

load("Haupttabellen.Rd")

t6[ t6$CHILD_MED_H_GLUCO_CORT == 1,]
