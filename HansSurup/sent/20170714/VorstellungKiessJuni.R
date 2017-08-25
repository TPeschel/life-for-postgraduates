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

tbl.thyroid.complete <-
    tbl[ !is.na( tbl$ ), ]

names( tbl )[ grep( "SD", names( tbl ) ) ]

names( tbl)




# 1. Deskription

# Wie viele Besuche gab es?

nrow(tbl)

# Wie viele männliche/ weibliche?
table( tbl$SEX )

# Ändere Namen

# ändere Namen

names( tbl.thyroid.complete )[ names( tbl.thyroid.complete ) == "C_ANTHRO_KH_AGE" ]        <- "AGE"
names( tbl.thyroid.complete )[ names( tbl.thyroid.complete ) == "C_ANTHRO_KH_EDAT" ]       <- "EDAT"
names( tbl.thyroid.complete )[ names( tbl.thyroid.complete ) == "C_ANTHRO_KH_GRP" ]        <- "SCIGROUP"
names( tbl.thyroid.complete )[ names( tbl.thyroid.complete ) == "TEILNEHMER_SIC" ]         <- "SIC"
names( tbl.thyroid.complete )[ names( tbl.thyroid.complete ) == "TEILNEHMER_GESCHLECHT" ]  <- "SEX"
names( tbl.thyroid.complete )[ names( tbl.thyroid.complete ) == "TEILNEHMER_GEB_JJJJMM" ]  <- "BIRTH"

tbl.thyroid.complete$SEX <- c( "male", "female")[ match( tbl.thyroid.complete$SEX, c( 1, 2 ) ) ]


##
# groupiere ueber sic und ermittle Reihenfolge der Besuche
##
tbl.thyroid.complete%<>% group_by( SIC )%>%mutate( VISIT = as.factor( dense_rank( EDAT ) ) ) 

tbl.thyroid.complete$AGE.CLASS <-cut(tbl.thyroid.complete$AGE, c( 0 : 18 ), c( 1 : 18 ) )



# Ansicht der Besuche
visits <-
  tbl.thyroid.complete %>%
  group_by( SIC, SEX ) %>%
  summarise( N = n( ) )

# Plotte Gesamtzahl der Besuche
ggplot( visits ) +
  geom_histogram( aes( N ), stat = "count" ) +
  facet_grid( . ~ SEX ) +
  scale_fill_brewer( type = "qual", palette = 3 ) +
  xlab( "total num of visits" ) +
  ylab( "count" ) +
  theme_bw( )

# tabelliere die Besuche pro Geschlecht
ggplot( tbl.thyroid.complete ) +
  geom_histogram( aes( AGE.CLASS, fill = VISIT ), stat = "count", position = "stack" ) +
  facet_grid( . ~ SEX ) +
  scale_fill_brewer( type = "qual", palette = 3 ) +
  xlab( "age up to n years" ) +
  ylab( "count of the n-the visit" ) +
  theme_bw( )

#Ermittlung des ersten Besuchs
tbl.only.1st.visit <-
  tbl.thyroid.complete[ tbl.thyroid.complete$VISIT == 1, ]

# GGplot des 1. Besuchs
ggplot( tbl.only.1st.visit ) +
  geom_histogram( aes( AGE.CLASS ), stat = "count" ) +
  facet_grid( . ~ SEX ) +
  scale_fill_brewer( type = "qual", palette = 3 ) +
  xlab( "age up to n years" ) +
  ylab( "count of 1st visits" ) +
  theme_bw( )


# Altersverteilung

describeBy(tbl.thyroid.complete$AGE, tbl.thyroid.complete$SEX)

# männliche Probanden Altersverteilung

table(tbl.thyroid.complete$SEX)

maennlicheProbanden <- subset(tbl.thyroid.complete, tbl.thyroid.complete$SEX == "male")

hist(maennlicheProbanden$AGE, xlim = c(0, 20), ylim = c(0, 400), col = "blue", breaks = 20)

# weibliche Probanden: Altersverteilung

weiblicheProbanden <- subset(tbl.thyroid.complete, tbl.thyroid.complete$SEX == "female")

hist(weiblicheProbanden$AGE, xlim = c(0, 20), ylim = c(0, 400), col = "red", breaks = 20)

# ein Altersbereich 10 bis 11 y der Jungen


maleover10 <- subset(maennlicheProbanden, maennlicheProbanden$AGE>10)
male10to11 <- subset(maleover10, male9to10$AGE<11)

# Darstellung der TSH- Werte, BMI- Wert sieser Untergruppe

describe(male10to11$TSH_S_NUM_VALUE)
hist(male10to11$TSH_S_NUM_VALUE, xlim = c(0, 10), ylim = c(0, 50), col = "blue", breaks = 10)

describe(male10to11$C_ANTHRO_KH_BMI_ORIG)
hist(male10to11$C_ANTHRO_KH_BMI_ORIG, xlim = c(10, 35), ylim = c(0, 70), col = "blue", breaks = 15)

# keine Normal Verteilung -> log- Transformation

male10to11$LOG10_TSH_S_NUM_VALUE <- log10( male10to11$TSH_S_NUM_VALUE )

male10to11$LOG10_ANTHRO_KH_BMI_ORIG <- log10( male10to11$C_ANTHRO_KH_BMI_ORIG )

# Erstellung eines ggplot, dort zeigt sich ein leichter Zusammenhang

( p1 <-
    ggplot( male10to11, aes( LOG10_ANTHRO_KH_BMI_ORIG, LOG10_TSH_S_NUM_VALUE) ) +
    geom_point( alpha = .2 ) +
    theme_bw( ) +
    scale_fill_manual( values = my.colors) +
    geom_smooth( method = "lm", alpha = .2 ) )

# Berechnung der Korrelation nach Pearson

corlogBMITSH <- cor.test(male10to11$LOG10_TSH_S_NUM_VALUE, male10to11$LOG10_ANTHRO_KH_BMI_ORIG, alternative = "two.sided", method = "pearson")
show(corlogBMITSH)


















