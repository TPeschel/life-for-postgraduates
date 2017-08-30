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
setwd( "~/LIFE/life-for-postgraduates/MaximilianeWagner/RAuswertung/" )
#setwd( "C:/Users/Anwender/Documents/Dissertation/RAuswertung/" )

load( "daten/main.table.Rd" )

main.table.complete.cortisol <-
	main.table[ !is.na( main.table$CORTISOL ), ]


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
# TANNER 392
# BMI, BMI-SDS 21 
# HEIGHT 19
# ...


##
# Ubersicht nach Geschlechtern
##
sapply( at[ at$SEX == "male", ], function( col ) { sum( is.na( col ) ) } )
sapply( at[ at$SEX == "female", ], function( col ) { sum( is.na( col ) ) } )

##
# Da es beim Tanner Missings gibt, 
# musst Du, bevor Du nach einem bestimmten Wert filterst,
# schauen, ob es ueberhaupt einen gibt
# Sonst suchst Du alle Zeilen mit Tanner == NA und Tanner == 1 aus
# Hier der Unterschied:
##
at[ at$TANNER == 1, ]
# wie Du siehst, werden Zeilen eingefuegt, die nur NAs enthalten und rausmuessen

# hier sind diese weg
at[ !is.na( at$TANNER ) & at$TANNER == 1, ]

##
# also, wenn die Filtervariable Missings enthaelt, diese Herausfiltern!!!
##
# Nicht So!
sapply( at[ at$TANNER == 1, ], function( col ) { sum( is.na( col ) ) } )

# Sonder So!
sapply( at[ !is.na( at$TANNER ) & at$TANNER == 1, ], function( col ) { sum( is.na( col ) ) } )

# und wieder die Nicht-Missings
# hier isses allerdings egal, dass Du nicht auf NA testest,
# da Du nur Nicht-Missings zaehlst und somit die erzeugten keine Rolle spielen
sapply( at[ at$TANNER == 5, ], function( col ) { sum( !is.na( col ) ) } )
sapply( at[ !is.na( at$TANNER ) & at$TANNER == 5, ], function( col ) { sum( !is.na( col ) ) } )

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


##
# Nochmal ein Beispiel
# welche Infos stecken in der Tabelle bzw fehlen fuer
# Jungs 
# mit einem Tanner von 2
# die untergewichtig sind
# und fuer die es einen Kortisol-Wert gibt?
##

auswahl <-
	at$SEX == "male" & !is.na( at$TANNER ) & at$TANNER == 2 &  at$BMI_ADJ < -1.28 & !is.na( at$CORTISOL )
	
at.ausw <-
	at[ auswahl, ]

sapply( at.ausw, function( col ) { sum( !is.na( col ) ) } )
sapply( at.ausw, function( col ) { sum( is.na( col ) ) } )

sapply( at.ausw, summary )

# so kannste vorher noch nach Nicht-Faktoren suchen
sapply( at.ausw, function( col ) { if( !is.factor( col ) ) summary( col ) } )

sapply( at.ausw, table )

# so kannste vorher noch nach Faktoren suchen
sapply( at.ausw, function( col ) { if( is.factor( col ) ) table( col ) } )




##
# PLOTS
##

at.plot <-
	main.table.complete.cortisol

summary( at.plot )

at.plot$AGE.CAT <-
	cut(
		at.plot$AGE,
		c( -1 : 20 ),
		c( +0 : 20 ) )

at.plot.sum <-
	at.plot %>% 
	group_by( 
		SEX,
		AGE.CAT ) %>%
	summarise(
		N = n( ) )

ggplot( at.plot.sum ) +
	theme_classic( ) +
	scale_color_manual( values = c( "deeppink", "deepskyblue" ) ) +
	scale_fill_manual( values = c( "deeppink", "deepskyblue" ) ) +
	geom_histogram( aes( AGE.CAT, N, fill = as.factor( SEX ) ), stat = "identity" ) +
	facet_grid( SEX ~ . ) +
	geom_hline( yintercept = 30, linetype = 2 )

View( at.plot.sum )

