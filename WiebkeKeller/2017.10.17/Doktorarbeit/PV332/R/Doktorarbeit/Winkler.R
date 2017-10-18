rm( list = ls( ) )

library( dplyr )
library( ggplot2 )
library( readxl )
library( lubridate )

my.theme <-
  theme_bw( )

setwd("../../data/")

tbl.all <-
  read_excel( "PV0332_Gesamt_Join.xlsx" )

tbl.wnklr <-
  read_excel( "PV0332_D00177.xlsx" )

tbl.all$JAHR <-
  year( tbl.all$E_SDQ_EDAT )

tbl <-
  merge( 
    tbl.all,
    tbl.wnklr,
    by.x = c( "TEILNEHMER_SIC", "JAHR" ),
    by.y = c( "PSEUDONYM", "JAHR" ) )

tbl <-
  tbl[ !is.na( tbl$SCORE_FAM ), ]

tbl$sex <-
  factor( 
    tbl$TEILNEHMER_GESCHLECHT,
    levels = c( 1, 2 ),
    labels = c( "male", "female") )

tbl$wnklr <-
  cut( 
    tbl$SCORE_FAM,
    breaks = c( 0, 8, 14, 21 ),
    labels = c( "LOW", "MID", "HIGH" ) )

## ## Wieviele Besuche haben welchen Winklerindex?
table( tbl$wnklr, tbl$sex )

ggplot( 
  tbl, 
  aes( wnklr, fill = sex ) ) + 
  geom_bar( stat = "count" ) +
  facet_grid( . ~ sex ) +
  my.theme +
  scale_fill_brewer( type = "qual", palette = 6, direction = -1 )

## Ermittle zu jeder Familie einen Eintrag fuer den Famscore aus
#Mittelwert aller
tbl.fam <-
  tbl %>%
  group_by( FAM_PSEUDO ) %>%
  summarise( score.fam = mean( SCORE_FAM ) )

## Gruppiere in LOW, MID, HIGH
tbl.fam$wnklr <-
  cut( 
    tbl.fam$score.fam,
    breaks = c( 0, 8, 14, 21 ),
    labels = c( "LOW", "MID", "HIGH" ) )

## Wieviele Familien haben welchen Winklerindex?
table( tbl.fam$wnklr )

ggplot( 
  tbl.fam, 
  aes( wnklr ) ) + 
  geom_bar( stat = "count" ) +
  my.theme

## Ermittle zu jedem Kind einen Eintrag fuer den Famscore aus 
#Mittelwert aller
tbl.knd <-
  tbl %>%
  group_by( TEILNEHMER_SIC, sex ) %>%
  summarise( score.fam = mean( SCORE_FAM ) )

## Gruppiere in LOW, MID, HIGH
tbl.knd$wnklr <-
  cut( 
    tbl.knd$score.fam,
    breaks = c( 0, 8, 14, 21 ),
    labels = c( "LOW", "MID", "HIGH" ) )

## Wieviele Kinder haben welchen Winklerindex?
table( tbl.knd$wnklr, tbl.knd$sex )

ggplot( 
  tbl.knd, 
  aes( wnklr, fill = sex ) ) + 
  geom_bar( stat = "count" ) +
  facet_grid( . ~ sex ) +
  theme_bw( ) +
  scale_fill_brewer( type = "qual", palette = 6, direction = -1 )






++
  
  
  
  
  
  
  
  
  
  
  



















+
  ++
  
  
  
  
  
  
  
  ++
  ++++++++
  ++
  
  
  +
  
  
  
  +++
  
  
  
  
  
  
  
  +
  +
  +++++++++++++++++++++++++++
  
  
  
  
  
  
  
  
  
  
  











































































































































































































































































































































