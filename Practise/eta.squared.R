##
# SPSS-Beispiel in R
# http://www.methodenberatung.uzh.ch/de/datenanalyse/unterschiede/zentral/mvarianz.html
##

##
# Mehrfaktorielle Varianzanalyse ohne Messwiederholungen
##

# loesche alles
############################################################
rm( list = ls( ) )
############################################################

# installiere und lade noetige Pakete
# foreign fuer read.spss
# lsr fuer eta²
############################################################
hlpr4life::load.pkgs( c( "foreign", "lsr" ) )
############################################################

# lese Datensatz ein
############################################################
( dat <- as.data.frame( read.spss( "ANOVA_mehrfaktoriell.sav" ) ) )
############################################################


# zeige Ergebnis der linearen Regression an
############################################################
summary( lm.1 <- lm( Stundenlohn ~ Berufserfahrung + Geschlecht, dat ) )
summary( lm.2 <- lm( Stundenlohn ~ Berufserfahrung * Geschlecht, dat ) ) # Modell auf Internetseite
############################################################

##
# Ancova
##

( anova( lm.1, lm.2 ) )

##
# Das bessere Modell ist das 2.
##

##
# ermittle partielle Effektstaerken
# eta.sq.part ist die interessant Spalte
# dort findest Du die Werte aus dem
# Beispiel auf der inet-Seite
############################################################
etaSquared( lm.2 )
############################################################
summary( lm.2 )

##
# Die einzige Groesse, die man nicht findet ist das ETA² zu konstanter Term (0.998),
# das wohl dem des Intercepts entspricht.
# Dieser interessiert Dich aber sowieso nicht so sehr 
## 