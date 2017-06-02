##
# loesche speicher
##
rm( list = ls( ) )

##
# setze Pfad zu aktuellem R-Arbeitsverzeichnis
setwd( "~/LIFE/life-for-postgraduates/LeaOelkers/data/" )

##
# lade Grafik Paket
##
library( ggplot2 )

##
# lade dplyr Paket
##
library( dplyr )

##
# bevorzugtes Schema fuer Grafiken
##
my.theme <-
    theme_bw( )

##
# lade Gesamtjoin
##
load( "tabelle.Rd" )

##
# Anzahl aller Besuche
##
nrow( tbl )

##
# Anzahl der Probanden
##
length( unique( tbl$SIC ) )

##
# erzeuge kleinere Tabelle, die nur Zeilen mit gueltigen FSH-Werten enthaelt
##
tbl.fsh <-
    tbl[ !is.na( tbl$FSH_S_VALUE_FLAG ) & ( tbl$FSH_S_VALUE_FLAG == 1 ) & !is.na( tbl$FSH_S_NUM_VALUE ), ]

##
# Altersbereich
##
summary( tbl.fsh$AGE )

##
# maennliche und weiliche Besuche mit FSH
##
table( tbl.fsh$SEX )

##
# Pseudoaggregierungsfunktion fuer die Variable SEX
##
fun <-
    function( v ) v[ 1 ]

##
# ermittle fuer jede SIC einmal das Geschlecht
##
tbl.sex <-
    tbl.fsh %>%
    group_by( SIC ) %>%
    summarise( sex = fun( SEX ) )

##
# maennliche und weiliche Besucher mit FSH
##
table( tbl.sex$sex ) 
rm( "tbl.sex" )

##
# Besuche, Kinder
##
length( tbl.fsh$SIC )
length( unique( tbl.fsh$SIC ) )

##
# erstelle Altersgruppen
##
tbl.fsh$AGE.CATEGORIE <-
    cut( 
        x = tbl.fsh$AGE,
        c( 0 : 20 ) )

##
# plotte Anzahlen der Besuche nach Geschlecht
##
ggplot( tbl.fsh ) +
    my.theme +
    scale_fill_manual( name = "Geschlecht", values = c( "red", "blue" ) ) +
    xlab( "Altersklassen" ) +
    ylab( "Anzahl der Besuche" ) +
    geom_histogram( aes( AGE.CATEGORIE, fill = SEX ), stat = "count" ) +
    facet_grid( . ~ SEX )

##
# ermittle fuer jede SIC Nummer der Besuche
##
tbl.visits <-
    tbl.fsh %>%
    group_by( SIC ) %>%
    mutate( VISIT = as.factor( dense_rank( EDAT ) ) )

##
# plotte Anzahlen der Kinder nach Geschlecht
##
ggplot( tbl.visits ) +
    my.theme +
    scale_fill_manual( name = "Besuche", values = c( "#ffe0e0", "#ffb0b0", "#ff8080", "#ff5050" ) ) +
    xlab( "Altersklassen" ) +
    ylab( "Anzahl der Besuche" ) +
    geom_histogram( aes( AGE.CATEGORIE, fill = VISIT ), stat = "count", position = "stack" ) +
    facet_grid( . ~ SEX )

##
# ermittle fuer jede SIC Anzahl der Besuche
##
tbl.visits <-
    tbl.fsh %>%
    group_by( SIC ) %>%
    summarise( 
        VISITS = n( ), 
        SEX = fun( SEX ) )

( visits <-
    table( tbl.visits$SEX, tbl.visits$VISITS ) )

##
# plotte Anzahlen der Kinder nach Geschlecht
##
ggplot( ) +
    my.theme +
    scale_fill_manual( name = "Geschlecht", values = c( "red", "blue", "black" ) ) +
    xlab( "Anzahl der Besuche" ) +
    ylab( "Anzahl Kinder mit n Besuchen" ) +
    geom_histogram( data = tbl.visits, aes( VISITS, fill = SEX ), stat = "count" ) +
    facet_grid( . ~ tbl.visits$SEX )

