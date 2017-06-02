##
## Probenliste Kuration
## 

require( "readxl" )
require( "dplyr" )

probenliste <-
    read_excel( "20161220_Probenliste_AGa_20170522.xlsx" )

str( probenliste )

probenliste <-
    probenliste[ -1, ]

str( probenliste, list.len = ncol( probenliste ) )

( names.pl <-
    names( probenliste ) )

##
## wandle character in numerics um
probenliste$durchgeführt <-
    as.numeric( probenliste$durchgeführt )

probenliste$`Haare gefärbt` <-
    as.numeric( probenliste$`Haare gefärbt` )

probenliste$Haargel <-
    as.numeric( probenliste$Haargel )

probenliste$Lokalisation <-
    as.numeric( probenliste$Lokalisation )

probenliste$Einwaage <-
    as.numeric( probenliste$Einwaage )

##
## bennene merkwuerdige Namen um
##
names( probenliste )[ names( probenliste ) == "durchgeführt" ] <-
    "durchgefuehrt"

names( probenliste )[ names( probenliste ) == "Haare gefärbt" ] <-
    "HaareGefaerbt"

names( probenliste )[ names( probenliste ) == "Länge Strähne [mm]" ] <-
    "StraehnenLaenge"

names( probenliste )[ names( probenliste ) == "Wie oft gewaschen" ] <-
    "Waschungen"

##
## zeige Struktur
##
str( probenliste, list.len = ncol( probenliste ) )

