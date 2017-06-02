## JOINT
## vereinige alle 4 Tabellen
##
rm( list = ls( ) )

library( "readxl" )
library( "WriteXLS" )
library( "dplyr" )
library( "ggplot2" )
library( "lifecuration" )

##
## Setze Pfad zum aktuellen Daten-Verzeichnis
## 
setwd( "~/LIFE/life-for-postgraduates/MaximilianeWagner/data/20170522/" )

source( "../../r/Join/data.join.curation.R" )
source( "../../r/Join/probenliste.curation.R" )
source( "../../r/Join/winkler.curation.R" )
