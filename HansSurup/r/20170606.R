##
# loesche alles
#
rm( list = ls( ) )

##
# lade alle noetigen Pakete
##
library( ggplot2 )
library( dplyr )
library( readxl )

my.colors <-
    rgb( runif( 32, 0, 1 ), runif( 32, 0, 1 ), runif( 32, 0, 1 ) )

##
# setze Pfade
##
setwd( "~/LIFE/life-for-postgraduates/HansSurup/data/" )

##
# lade Tabelle 
##
tbl <- read_excel( "PV0365_Gesamt_Join.xlsx" )

##
# ermittle, welches AGE und EDAT die meisten Eintraege enthaelt
##
sapply(
    grep( "AGE", names( tbl ) ),
    function( n ) c( names( tbl )[ n ], sum( is.na( tbl[ , n ] ) ) ) )

sapply(
    grep( "EDAT", names( tbl ) ),
    function( n ) c( names( tbl )[ n ], sum( is.na( tbl[ , n ] ) ) ) )

# ANTHRO_KH

##
# aendere Namen
##
names( tbl )[ names( tbl ) == "C_ANTHRO_KH_AGE" ]        <- "AGE"
names( tbl )[ names( tbl ) == "C_ANTHRO_KH_EDAT" ]       <- "EDAT"
names( tbl )[ names( tbl ) == "C_ANTHRO_KH_GRP" ]        <- "SCIGROUP"
names( tbl )[ names( tbl ) == "TEILNEHMER_SIC" ]         <- "SIC"
names( tbl )[ names( tbl ) == "TEILNEHMER_GESCHLECHT" ]  <- "SEX"
names( tbl )[ names( tbl ) == "TEILNEHMER_GEB_JJJJMM" ]  <- "BIRTH"

nms <- c( "SIC", "SCIGROUP", "EDAT", "SEX", "BIRTH", "AGE" )
nms <- c( nms, setdiff( names( tbl), nms ) )

tbl <- tbl[ ,nms ]

##
# aendere Kodierung fuer SEX
##
tbl$SEX <- c( "male", "female")[ match( tbl$SEX, c( 1, 2 ) ) ]

##
# schaue 34 missings an
##
View( tbl[ is.na( tbl$AGE ), ] )

##
# raus mit!
##
tbl <- tbl[ !is.na( tbl$AGE ), ]



###################
#                 #
# 1st DESKRIPTION #
#                 #
###################

# wieviele Besuche?
##
nrow( tbl )

##
# wieviele maennliche und weibliche Besuche?
##
table( tbl$SEX )

##
# groupiere ueber sic und ermittle Reihenfolge der Besuche
##
tbl %<>%
    group_by( SIC ) %>%
    mutate( VISIT = as.factor( dense_rank( EDAT ) ) )

##
# erstelle Altersklassen fuer Histogramm
##
tbl$AGE.CLASS <-
    cut(
        tbl$AGE,
        c( 0 : 18 ),
        c( 1 : 18 ) )

##
# tabelliere Besucher pro Geschlecht
##
table( tbl$VISIT, tbl$SEX )

ggplot( tbl ) +
    geom_histogram( aes( AGE.CLASS, fill = VISIT ), stat = "count", position = "stack" ) +
    facet_grid( . ~ SEX ) +
    scale_fill_brewer( type = "qual", palette = 3 ) +
    xlab( "age up to n years" ) +
    ylab( "count of the n-the visit" ) +
    theme_bw( )

##
# Schaue Besuche an
##
visits <-
    tbl %>%
    group_by( SIC, SEX ) %>%
    summarise( N = n( ) )

##
# plotte Gesamtanzahl Besuche
##
ggplot( visits ) +
    geom_histogram( aes( N ), stat = "count" ) +
    facet_grid( . ~ SEX ) +
    scale_fill_brewer( type = "qual", palette = 3 ) +
    xlab( "total num of visits" ) +
    ylab( "count" ) +
    theme_bw( )

##
# ermittle nur erste Besuche
##
tbl.only.1st.visit <-
    tbl[ tbl$VISIT == 1, ]

ggplot( tbl.only.1st.visit ) +
    geom_histogram( aes( AGE.CLASS ), stat = "count" ) +
    facet_grid( . ~ SEX ) +
    scale_fill_brewer( type = "qual", palette = 3 ) +
    xlab( "age up to n years" ) +
    ylab( "count of 1st visits" ) +
    theme_bw( )


##
# ermittle nur erste Besuche
##
tbl.only.2nd.visit <-
    tbl[ tbl$VISIT == 2, ]


ggplot( tbl.only.2nd.visit ) +
    geom_histogram( aes( AGE.CLASS ), stat = "count" ) +
    facet_grid( . ~ SEX ) +
    scale_fill_brewer( type = "qual", palette = 3 ) +
    xlab( "age up to n years" ) +
    ylab( "count of 1st visits" ) +
    theme_bw( )

##
# Suche nach Werten
##
## falls es ein flag gibt, fasse zunaechst das zusammen
summary( tbl$TSH_S_VALUE_FLAG )
# 205 Missings
summary( tbl$TSH_S_NUM_VALUE )
# 351 Missings

# erstelle neue Tabelle, in der in jeder Zeile ein TSH_VALUE vorliegt 
tbl.tsh.complete <-
    tbl[ !is.na( tbl$TSH_S_NUM_VALUE ), ]

# wieviele haben kein Flag
nrow( tbl.tsh.complete[ is.na( tbl.tsh.complete$TSH_S_VALUE_FLAG ), ] )

# tabelliere mal die Flags 
table( tbl.tsh.complete$TSH_S_VALUE_FLAG )

# codes aus ACRF in Text umgewandelt
table( c( "kleiner als", "gleich", "groesser als" )[ match( tbl.tsh.complete$TSH_S_VALUE_FLAG, c( 0, 1, 2 ) ) ] )

# ein Wert groesser einer kleiner
summary( tbl.tsh.complete$TSH_S_NUM_VALUE )

# zu bunt! jede Farbe ist ein Alter
ggplot( tbl.tsh.complete ) +
    geom_histogram( 
        aes( 
            cut( 
                tbl.tsh.complete$TSH_S_NUM_VALUE,
                c( seq( 0, 10, by = .1 ), Inf ),
                seq( 0, 10, by = .1 ) ),
            fill = AGE.CLASS ),
        stat = "count" ) +
    facet_grid( . ~ SEX ) +
    theme_bw( ) +
    scale_fill_manual( values = my.colors, "age classes" ) + 
    scale_x_discrete( breaks = c( 0 : 10 ), "TSH" )

# jez mal als Scatterplot
ggplot( tbl.tsh.complete ) +
    geom_point( 
        aes( AGE, TSH_S_NUM_VALUE, col = AGE.CLASS ), alpha = .2 ) +
    facet_grid( . ~ SEX ) +
    theme_bw( ) +
    scale_color_manual( values = my.colors, "age" )

# sieh mal die Ausreisser an
View( tbl.tsh.complete[ tbl.tsh.complete$TSH_S_NUM_VALUE >= 30, ] )

# schmeisse so alle Ausreiser raus / behalte alle, die einen TSH-Wert < 30 haben // REIN FIKTIV!!!
tbl.tsh.complete <- tbl.tsh.complete[ tbl.tsh.complete$TSH_S_NUM_VALUE < 30, ]

# ermittle dekadischen Logarithmus des TSH-Wertes
tbl.tsh.complete$LOG10_TSH_S_NUM_VALUE <- log10( tbl.tsh.complete$TSH_S_NUM_VALUE )

# ermittle dekadischen Logarithmus des BMI-Wertes
tbl.tsh.complete$LOG10_ANTHRO_KH_BMI_ORIG <- log10( tbl.tsh.complete$C_ANTHRO_KH_BMI_ORIG )

# fasse zusammen
summary( tbl.tsh.complete$LOG10_TSH_S_NUM_VALUE )

# plotte
ggplot( tbl.tsh.complete ) +
    geom_point( 
        aes( AGE, LOG10_TSH_S_NUM_VALUE ), alpha = .2 ) +
    facet_grid( SEX ~ . ) +
    theme_bw( ) +
    scale_fill_manual( values = my.colors, "age" ) +
    ylim( -1, 1 )
 

# histogramm zu log10( TSH )
ggplot( tbl.tsh.complete ) +
    geom_histogram( 
        aes( 
            cut( 
                LOG10_TSH_S_NUM_VALUE,
                seq( -2, +2, .1 ),
                seq( -1.9, +2, .1 ) ),
            fill = AGE.CLASS ),
        stat = "count" ) +
    facet_grid( . ~ SEX ) +
    theme_bw( ) +
    scale_fill_manual( values = my.colors, "age classes" )

ggplot( tbl.tsh.complete, aes( TSH_S_NUM_VALUE, C_ANTHRO_KH_BMI_ORIG, col = AGE.CLASS ) ) +
    geom_point( alpha = .2 ) +
    facet_grid( . ~ SEX ) +
    theme_bw( ) +
    scale_fill_manual( values = my.colors, "age classes" ) 

( p1 <-
    ggplot( tbl.tsh.complete, aes( LOG10_ANTHRO_KH_BMI_ORIG, LOG10_TSH_S_NUM_VALUE, col = AGE.CLASS ) ) +
        geom_point( alpha = .2 ) +
        facet_grid( . ~ SEX ) +
        theme_bw( ) +
        scale_fill_manual( values = my.colors, "age classes" ) +
        geom_smooth( method = "lm", alpha = .2 ) )

( lm.log10tsh.bmi.male <-
    lm( data = tbl.tsh.complete[ tbl.tsh.complete$SEX == "male", ], LOG10_TSH_S_NUM_VALUE ~ LOG10_ANTHRO_KH_BMI_ORIG ) )

( lm.log10tsh.bmi.female <-
    lm( data = tbl.tsh.complete[ tbl.tsh.complete$SEX == "female", ], LOG10_TSH_S_NUM_VALUE ~ LOG10_ANTHRO_KH_BMI_ORIG ) )

summary( lm.log10tsh.bmi.male )
summary( lm.log10tsh.bmi.female )

df.helper <-
    data.frame( 
        SEX = c( "male", "female" ),
        a = c( lm.log10tsh.bmi.male$coefficients[ 1 ], lm.log10tsh.bmi.female$coefficients[ 1 ] ),
        b = c( lm.log10tsh.bmi.male$coefficients[ 2 ], lm.log10tsh.bmi.female$coefficients[ 2 ] ) )

p1 + geom_abline( data = df.helper, aes( intercept = a, slope = b, group = SEX ) )


( p2 <-
    ggplot( tbl.tsh.complete, aes( LOG10_TSH_S_NUM_VALUE, LOG10_ANTHRO_KH_BMI_ORIG, col = AGE.CLASS ) ) +
        geom_point( alpha = .2 ) +
        facet_grid( . ~ SEX ) +
        theme_bw( ) +
        scale_fill_manual( values = my.colors, "age classes" ) +
        geom_smooth( method = "lm", alpha = .2 ) )


( lm.log10tsh.bmi.male <-
    lm( data = tbl.tsh.complete[ tbl.tsh.complete$SEX == "male", ], LOG10_ANTHRO_KH_BMI_ORIG ~ LOG10_TSH_S_NUM_VALUE ) )

( lm.log10tsh.bmi.female <-
    lm( data = tbl.tsh.complete[ tbl.tsh.complete$SEX == "female", ], LOG10_ANTHRO_KH_BMI_ORIG ~ LOG10_TSH_S_NUM_VALUE ) )

summary( lm.log10tsh.bmi.male )
summary( lm.log10tsh.bmi.female )

df.helper <-
    data.frame( 
        SEX = c( "male", "female" ),
        a = c( lm.log10tsh.bmi.male$coefficients[ 1 ], lm.log10tsh.bmi.female$coefficients[ 1 ] ),
        b = c( lm.log10tsh.bmi.male$coefficients[ 2 ], lm.log10tsh.bmi.female$coefficients[ 2 ] ) )

p2 + geom_abline( inherit.aes = F, data = df.helper, aes( intercept = a, slope = b, group = SEX ) )

