# Fragestellung: Unterscheidet sich der Verlauf der Calcitoninkonzentration von Kindern 
# mit niedrigem und normalem Geburtsgewicht?
# Unterscheidet sich de Differenz der Calcitoninwerte zwischen zwei Besuchen bei Kindern 
# mit niedrigem/normalen Geburtsgewicht (Alter 0-0.75, somit ja max.2 Besuche)?
# Findet bei Kindern mit niedrigem Geburtsgewicht ein größerer Abfall
# in der Calcitoninkonzentration statt als bei Kindern mit normalem Geburtsgewicht?
# Kinder mit niedrigem Geburtsgewicht haben wahrscheinlich höhere Calcitoninwerte bei der Geburt
# Interessant zu sehen, wie der weitere Verlauf der Calcitoninkonzentration ist

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


#AGE <-
  #cut(
    #d$AGE_Calcitionin,
    #c( 0 : 20 ) )

d <-
  d[ !is.na( d$AGE_Calcitionin ) & !is.na( d$CT_S_1_NUM_VALUE )& !is.na( d$U1_GEW ), ]
summary(d$U1_GEW)

d.m <- d[ d$TEILNEHMER_GESCHLECHT=="male", ]
d.f <- d[ d$TEILNEHMER_GESCHLECHT=="female", ]

d0075 <- d[between(d$AGE_Calcitionin, 0,0.75),]

d0075m <- d.m[between(d.m$AGE_Calcitionin, 0,0.75),]
d0075f <- d.f[between(d.f$AGE_Calcitionin, 0,0.75),]
summary(d0075m$U1_GEW)
summary(d0075f$U1_GEW)

# Überlegung --> nieriges Geburtsgewicht <= 2500g
#d0075m.aus <- d0075m[ d0075m$U1_GEW <= 2500, ] sind nur 4
#d0075f.aus <- d0075f[ d0075f$U1_GEW <= 2500, ] sind nur 4

d0075 <-
  arrange( d0075, CT_S_1_SIC ) %>%
  group_by( CT_S_1_SIC, TEILNEHMER_GESCHLECHT ) %>%
  mutate( 
    diff.visit = as.numeric( difftime( CT_S_1_DATUM, lag( CT_S_1_DATUM ), units = "days" ) ) / 365.25,
    diff.ct_val = CT_S_1_NUM_VALUE - lag( CT_S_1_NUM_VALUE ),
    rat.ct_val.vis = diff.ct_val / diff.visit )



ggplot( d0075 ) +
  geom_point( aes( U1_GEW, rat.ct_val.vis, col = TEILNEHMER_GESCHLECHT ) )

d0075m <-
  arrange( d0075m, CT_S_1_SIC ) %>%
  group_by( CT_S_1_SIC ) %>%
  mutate( 
    diff.visit = as.numeric( difftime( CT_S_1_DATUM, lag( CT_S_1_DATUM ), units = "days" ) ) / 365.25,
    diff.ct_val = CT_S_1_NUM_VALUE - lag( CT_S_1_NUM_VALUE ),
    rat.ct_val.vis = diff.ct_val / diff.visit )


ggplot( d0075m ) +
  geom_point( aes( U1_GEW, rat.ct_val.vis) )


d0075f <-
  arrange( d0075f, CT_S_1_SIC ) %>%
  group_by( CT_S_1_SIC ) %>%
  mutate( 
    diff.visit = as.numeric( difftime( CT_S_1_DATUM, lag( CT_S_1_DATUM ), units = "days" ) ) / 365.25,
    diff.ct_val = CT_S_1_NUM_VALUE - lag( CT_S_1_NUM_VALUE ),
    rat.ct_val.vis = diff.ct_val / diff.visit )


ggplot( d0075f ) +
  geom_point( aes( U1_GEW, rat.ct_val.vis) )

#Probleme
#zu wenige; die mit niedrigerem Geburtsgewicht waren meistens im ersten Lebenjahr nicht 
#noch einmal da
#es lässt sich kein Trend erkennen (wahrscheinlich kein Unterschied im Verlauf)
#Frage: Müsste man dies nicht auch mit Calcitonin_SDS-Werten berechnen?


# die ersten zwei Besuche graphisch darstellen, durch eine Linie verbinden
# y-achse -> Calcitoninkonzentration
# x-achse -> Geburtsgewicht
# Punkt des ersten Besuchs sollte andere Farbe haben als Punkt des zweiten Besuchs
# --> dieses sollte in Legende vermerkt werden
summary(d0075$AGE_Calcitionin)
summary(d0075m$AGE_Calcitionin)
summary(d0075f$AGE_Calcitionin)
#am besten Alter durch Punkt dargestellt werden -> blau = 0.25 J; rot=0.5 bzw. bis 0.41 und größer 0.41

AGE <-
cut(
d0075$AGE_Calcitionin,
c( 0,0.25,0.5,0.75 ) )

ggplot( d0075 ) +
  geom_path( aes( U1_GEW, CT_S_1_NUM_VALUE, col = TEILNEHMER_GESCHLECHT, group = TEILNEHMER_GESCHLECHT ) ) +
  geom_point( aes( U1_GEW, CT_S_1_NUM_VALUE, col = TEILNEHMER_GESCHLECHT) )
## ist noch nicht so, wie ich es mir vorgestellt habe

# wahrscheinlich die Verfolgung der Idee nicht möglich, kaum Kinder mit niedrigem Geburtsgewicht
# und die meisten davon dann kein zweites Mal in der Studienambulanz


# t.test zum Geburtsgewicht
# 0-0.375 Jahre --> 1. Termin
#Calcitoninkonzentration und Geburtsgewicht --> 
#Ist die Calcitoninkonzentration höher bei Kindern mit niedrigem Geburtsgewicht?
#Calcitoninkonzentratin/Alter --> Calcitonin_SDS


 
