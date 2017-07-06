# Wachstumsgeschwindigkeit berechnen
# ich dachte mir, dass dies auch so ähnlich funktionieren müsste, wie mit der Calcitonindifferenzberechnung
# allerdings denke ich jetzt, dass dies wahrscheinlich zu ungenau ist

rm( list = ls( ) )
library( dplyr )
library( readxl )
library( ggplot2 )
library(reshape2)


setwd( "~/Desktop/pv0116_neu/")

d <-
  read_excel( "Tabellen/AktuelleTabelle190517excel.xlsx" )

d$AGE_Calcitionin <-
  as.numeric( d$AGE_Calcitionin )
d <-
  d[ !is.na( d$AGE_Calcitionin ) & !is.na( d$CT_S_1_NUM_VALUE )& !is.na( d$C_ANTHRO_KH_HEIGHT_ORIG ), ]


d$AGE <-
  cut(
    d$AGE_Calcitionin,
    c( 0 : 20 ) )


s <- 
  d %>%
  group_by( TEILNEHMER_GESCHLECHT, AGE ) %>%
  summarise( groesse = mean( C_ANTHRO_KH_HEIGHT_ORIG ) )

arrange( s, TEILNEHMER_GESCHLECHT, AGE )

s$dff.groesse <- s$groesse - lag( s$groesse )


s$dff.groesse[ s$AGE == "(0,1)]" ] <- NA


ggplot( s ) + 
  geom_point( aes( AGE, dff.groesse, col  = TEILNEHMER_GESCHLECHT ) ) +
  geom_path( aes( AGE, dff.groesse, group = TEILNEHMER_GESCHLECHT, col  = TEILNEHMER_GESCHLECHT ) )

# aus irgendwelchen Gründe ist da bei 0,1 male ein Punkt

