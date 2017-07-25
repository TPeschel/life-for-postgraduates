rm( list = ls( ) )

library( "hlpr4life" )

load.pkgs(
	c(
		"readxl",
		"xlsx",
		"dplyr",
		"ggplot2",
		"ggthemes",
		"lubridate" ) )

# hier Deinen Pfad eintragen
setwd( "~/LIFE/life-for-postgraduates/MaximilianeWagner/RAuswertung/daten/erzeugt/" )
#setwd( "C:/Users/Anwender/Documents/Dissertation/RAuswertung/daten/erzeugt/" )

load( "allData.windsorized.and.curated.2017.07.25.Rd" )


##
# Arbeitstabelle at
# erstmal main.table zuweisen und alle Zusammenfassungen anschauen
# spaeter einfach 
# main.table.complete.cortisol zuweisen und denselben Kode danach wieder ausfuehren
##

# Zusammenfassungen fuer die Gesamttabelle
at <-
	main.table

# oder Zusammenfassungen fuer die Tabelle mit vollstaendigen Kortisolangaben
# at <-
# 	main.table.complete.cortisol


# so bekommst Du eine Uebersicht aller Missings respektive NichtMissings in allen Spalten auf einen Blick
sapply( at, function( col ) { sum( is.na( col ) ) } )
sapply( at, function( col ) { sum( !is.na( col ) ) } )

##
# Anzahl der Missings:
# SEX, SIC, AGE, MAT_NUM     keine
# CORTISOL 1616
# PUBSTAT 392
# BMI, BMI-SDS 21 
# HEIGHT 19
# ...


##
# Ubersicht nach Geschlechtern
##
sapply( at[ at$SEX == "male", ], function( col ) { sum( is.na( col ) ) } )
sapply( at[ at$SEX == "female", ], function( col ) { sum( is.na( col ) ) } )

##
# Da es beim PUBSTAT Missings gibt, 
# musst Du, bevor Du nach einem bestimmten Wert filterst,
# schauen, ob es ueberhaupt einen gibt
# Sonst suchst Du alle Zeilen mit PUBSTAT == NA und PUBSTAT == 1 aus
# Hier der Unterschied:
##
at[ at$PUBSTAT == 1, ]
nrow( at[ at$PUBSTAT == 1, ] )
# 1528 Zeilen
# es wird aber jedes mal wenn in der Variable PUBSTAT ein NA gefunden wird, 
# fuer dieses eine passende Zeile mit NAs erzeugt
# wie Du siehst, werden Zeilen eingefuegt, die nur NAs enthalten und rausmuessen
# das erkennst Du an den Zeilennummern (linke Spalte).
# Da entstehen NA.1, NA.2 usw


# UND
# hier sind diese weg
at[ !is.na( at$PUBSTAT ) & at$PUBSTAT == 1, ]
nrow( at[ !is.na( at$PUBSTAT ) & at$PUBSTAT == 1, ] )
# echte 1135 Zeilen
# das heisst at[ !is.na( at$PUBSTAT ) & at$PUBSTAT == 1, ] waehlt nur die Zeilen aus, 
# die auch wirklich den PUBSTAT gleich 1 haben und nicht noch zusaetzliche durch die Missings

##
# also, wenn die Filtervariable Missings enthaelt, diese Herausfiltern!!!
##
# Nicht So!
sapply( at[ at$PUBSTAT == 1, ], function( col ) { sum( is.na( col ) ) } )

# Sondern So!
sapply( at[ !is.na( at$PUBSTAT ) & at$PUBSTAT == 1, ], function( col ) { sum( is.na( col ) ) } )

# und wieder die Nicht-Missings
# hier isses allerdings egal, dass Du nicht auf NA testest,
# da Du nur Nicht-Missings zaehlst und somit die erzeugten keine Rolle spielen
sapply( at[ at$PUBSTAT == 5, ], function( col ) { sum( !is.na( col ) ) } )
sapply( at[ !is.na( at$PUBSTAT ) & at$PUBSTAT == 5, ], function( col ) { sum( !is.na( col ) ) } )

# so bekommst Du eine Uebersicht aller Nicht-Missings in allen Spalten auf einen Blick
# Test auf Missings bei SEX nicht noetig, da es keine gibt.
sapply( at, function( col ) { sum( !is.na( col ) ) } )
sapply( at[ at$SEX == "male", ], function( col ) { sum( !is.na( col ) ) } )
sapply( at[ at$SEX == "female", ], function( col ) { sum( !is.na( col ) ) } )

##
# Genau, wie die Summe der Missings,
# kannst Du fuer jede Spalte ein Summary ausfuehren lassen
##

sapply( at, summary )
sapply( at[ at$SEX == "male", ], summary )
sapply( at[ at$SEX == "female", ], summary )

sapply( at, function( col ) if( is.factor( col ) ) table( col ) )
sapply( at[ at$SEX == "male", ], function( col ) if( is.factor( col ) ) table( col ) )
sapply( at[ at$SEX == "male", ], function( col ) if( is.factor( col ) ) table( col ) )

##
# Nochmal ein Beispiel
# welche Infos stecken in der Tabelle bzw fehlen fuer
# Jungs 
# mit einem PUBSTAT von 2
# die untergewichtig sind
# und fuer die es einen Kortisol-Wert gibt?
##

sapply( at[ at$SEX == "male" & !is.na( at$PUBSTAT ) & at$PUBSTAT == 2 &  at$BMI_ADJ < -1.28 & !is.na( at$CORTISOL ), ], function( col ) { sum( !is.na( col ) ) } )
sapply( at[ at$SEX == "male" & !is.na( at$PUBSTAT ) & at$PUBSTAT == 2 &  at$BMI_ADJ < -1.28 & !is.na( at$CORTISOL ), ], function( col ) { sum( is.na( col ) ) } )

sapply( at[ at$SEX == "male" & !is.na( at$PUBSTAT ) & at$PUBSTAT == 2 &  at$BMI_ADJ < -1.28 & !is.na( at$CORTISOL ), ], summary )
# so kannste vorher noch nach Nicht-Faktoren suchen
sapply( at[ at$SEX == "male" & !is.na( at$PUBSTAT ) & at$PUBSTAT == 2 &  at$BMI_ADJ < -1.28 & !is.na( at$CORTISOL ), ], function( col ) { if( !is.factor( col ) ) summary( col ) } )

sapply( at[ at$SEX == "male" & !is.na( at$PUBSTAT ) & at$PUBSTAT == 2 &  at$BMI_ADJ < -1.28 & !is.na( at$CORTISOL ), ], table )
# so kannste vorher noch nach Faktoren suchen
sapply( at[ at$SEX == "male" & !is.na( at$PUBSTAT ) & at$PUBSTAT == 2 &  at$BMI_ADJ < -1.28 & !is.na( at$CORTISOL ), ], function( col ) { if( is.factor( col ) ) table( col ) } )

