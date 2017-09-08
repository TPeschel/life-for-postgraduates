rm( list = ls( ) )

if( !"devtools" %in% rownames( installed.packages( ) ) ) install.packages( "devtools" )

#devtools::install_github( "TPeschel/hlpr4life", force = T )

hlpr4life::load.pkgs( c( "hlpr4life", "ggplot2", "broom" ) )

###########################################################

##
# definiere ein paar plots fuer spaeter
# macht alles bequehmer
##
ggplot.histogram <-
	function( y, breaks = 30 ) {
		
		brks <-
			seq(
				min( y )-.00001,
				max( y ),
				length.out = breaks )
		
		lbls <-
			round( ( brks[ -1 ] + brks[ -length( brks ) ] ) / 2, 2 )
		
		cuts <- 
			cut( 
				y, 
				breaks = brks,
				labels = lbls )
		
		ggplot( ) +
			theme_bw( ) +
			theme(
				axis.text.x = element_text(
					angle = 90 ),
				axis.title = element_blank( ) ) +
			geom_histogram( 
				aes( cuts ),
				stat = "count" ) }

ggplot.scatter <-
	function( x, y, col = "black" ) {
		ggplot( ) +
			theme_bw( ) +
			geom_point(  aes( x, y, col = col ) ) }

set.seed( 1 )
###########################################################


##
# Ein kleines Beispiel zum Verstaendnis, was scale( ) macht
# scale standardisiert eine Verteilung, sodass deren Mittelwert 0 und 
# die Standardabweichung 1 werden
##
normal.verteilung.mittelwert.20.standardabweichung.10 <-
	rnorm( 1000, 20, 10 )

normal.verteilung.mittelwert.1.standardabweichung.1 <-
	scale( normal.verteilung.mittelwert.20.standardabweichung.10 )

ggsubplot(
	ggplot.scatter( c( 1: 1000 ), normal.verteilung.mittelwert.20.standardabweichung.10, 0 ) + scale_color_continuous( guide = F ) + coord_flip( ),
	ggplot.histogram( normal.verteilung.mittelwert.20.standardabweichung.10, 21 ),
	ggplot.scatter( c( 1: 1000 ), normal.verteilung.mittelwert.1.standardabweichung.1, 0 ) + scale_color_continuous( guide = F ) + coord_flip( ),
	ggplot.histogram( normal.verteilung.mittelwert.1.standardabweichung.1, 21 ),
	cols = 2 )

###########################################################


##
# Was hat es eigentlich mit der Varianz bei der linearen Regression auf sich?
##


##
# Unsere Tabelle enthaelt immer N = 1000 Messungen
##

N <-
	1000

##
# Nehmen wir an, wir betrachten das Verhaeltnis 
# von Groesse und Alter und haben eine Verteilung, 
# die nur sehr schwach im Alter streut aber 
# stark in den Groessen
##

# Nehmen wir eine Altersverteilung um das 10. Lebensjahr
# mit einer Streuung von ungefaehr 3.65 Tagen
d <-
	data.frame(
		age = rnorm( N, 10, .01 ) )

# Das Alter streut nur sehr wenig
sd( d$age ) * 365.25 # Tage

# Altersumfang in Jahren
# sehr klein
range( d$age )

# Alterspanne
( max( d$age ) - min( d$age ) ) * 365.25 # Tage


# Mit jedem Jahr soll die Groesse um 12.3 cm zunehmen
# Im Alter von 0 Jahren soll die Groesse 30 cm betragen
# und die Streuung soll 10cm betragen
# Da das Alter im Mittel um 10 Jahre liegt,
# sollten die Werte um 123 + 30 = 153 liegen
d$hgt <-
	12.3 * d$age + rnorm( N, 30, 10 )

# plotte das Ding
( 
	p <-
		ggplot.scatter( d$age, d$hgt, 1 ) )

# Regression ausfuehren,
# aber noch nicht besprechen
lm.hgt.age.1 <- 
	lm( hgt ~ age, d )

# erstmal Groesse anschauen
# Groessenspanne
max( d$hgt ) - min( d$hgt )# cm

# Groessenumfang
range( d$hgt ) #cm

# Groessenstreuung
sd( d$hgt ) #cm

# jetzt doch mal Regression ansehen?
lm.hgt.age.1$coefficients

anstieg.1 <-
	lm.hgt.age.1$coefficients[ "age" ]

# Aendert sich das Alter um eine Standardabweichung
(
	age.1.sd <- 
		sd( d$age ) )

# aendert sich die Groesse um 
anstieg.1 * age.1.sd

# Das ist sehr wenig im Verhaeltnis 
# zur Streuung der Groesse selbst von 
(
	hgt.1.sd <-
		sd( d$hgt ) )

# das Verhaeltnis ist 
100 * anstieg.1 * age.1.sd / hgt.1.sd # Prozent

# Das sollte es schwer machen, die Regressionsgerade 
# mit blossem Auge zu finden
p + geom_smooth( aes( d$age, d$hgt ), method = "lm" )

# Die Ergebnisse der Regression geben das auch wieder.
# Weder sind die Koeffizienten signifikant, 
# noch stimmen sie ueberhaupt.
# Der Intercept sollte 30 sein und age 12.3 .
tidy( lm.hgt.age.1 )

##
# jetzt standardisieren wir die beiden Verteilungen
# sodass die Koeffizienten jetzt statt b's beta's sind
# welche uns den Einfluss der Groesse von -1 bis +1 ( bei unkorrelierten Daten )
# angeben
##
d.scaled <-
	as.data.frame(
		sapply( d, scale ) )

##
# Der Intercept ist hier egal, da eh = 0
# Hier siehst Du, dass der Einfluss der Alters auf 
# die Groesse gerade mal 0.03 also ungefaehr 3% sind
# Sprich das Alter erklaert die Groesse zu 3%
# Deswegen ist es auch nicht signifikant
##
tidy( lm.hgt.age.scaled.1 <- lm( hgt ~ age, d.scaled ) )

(
	p <-
		ggplot.scatter( d.scaled$age, d.scaled$hgt, 1 ) )

p + geom_smooth( aes( d.scaled$age, d.scaled$hgt ), method = "lm" )

##
# Nun das gleiche nochmal
# Jetzt aber mit einer groesseren Streuung 
# des Alters im Verhaeltnis zur Streuung der Groesse
##

# jetzt soll das Alter um ein Jahr streuen
d2 <-
	data.frame(
		age = rnorm( 1000, 10, 1 ) )

d2$hgt <-
	12.3 * d2$age + rnorm( N, 30, 10 )

( 
	p <-
		ggplot.scatter( d2$age, d2$hgt, 1 ) )

# wieder Regression ausfuehren,
# aber noch nicht besprechen
lm.hgt.age.2 <- 
	lm( hgt ~ age, d2 )

# Das Alter streut jetzt ungefaehr um ein Jahr
( 
	age.2.sd <- 
		sd( d2$age ) )

range( d2$age )
max( d2$age ) - min( d2$age )

# jetzt wieder Regression ansehen?

lm.hgt.age.2$coefficients

anstieg.2 <-
	lm.hgt.age.2$coefficients[ "age" ]

# wie stark aendert sich die Groesse wenn sich das Alter um 
# eine Standardabweichung aendert?
anstieg.2 * age.2.sd

# das ist fast so gross, wie die Streuung der Groesse selbst
( 
	hgt.2.sd <-
		sd( d2$hgt ) )

100 * anstieg.2 * age.2.sd / hgt.2.sd # Prozent

# Das sollte es leicht machen, die Regressionsgerade 
# mit blossem Auge zu finden
p + geom_smooth( aes( d2$age, d2$hgt ), method = "lm" )

# Die Ergebnisse der Regression geben das auch wieder.
# Die Koeffizienten sind signifikant und stimmen ungefaehr.
# Der Intercept sollte 30 sein und age 12.3 .
tidy( lm.hgt.age.2 )

##
# jetzt standardisieren wir die beiden Verteilungen wieder
##
d2.scaled <-
	as.data.frame(
		sapply( d2, scale ) )

(
	p <-
		ggplot.scatter( d2.scaled$age, d2.scaled$hgt, 1 ) )

##
# Der Intercept ist hier egal, da eh = 0
# Hier siehst Du, dass der Einfluss des Alters auf 
# die Groesse jetzt fast 63% sind 
# Das sollte es leicht machen,
# mit blossem Auge eine Gerade zu schaetzen
##
tidy( lm.hgt.age.scaled.2 <- lm( hgt ~ age, d2.scaled ) )

p + geom_smooth( aes( d2.scaled$age, d2.scaled$hgt ), method = "lm" )


# Vielleicht ist Dir aufgefallen,
# dass das Verhaeltnis von 
# anstieg * sd( age ) / sd( hgt )
# die Koeffizienten der standardisierten
# Regression sind und sich
# in der Regressionsuebersicht wiederfinden
# bei unkorrelierten Daten sind es auch
# die Korrelationswerte

anstieg.1 * age.1.sd / hgt.1.sd 
tidy( lm.hgt.age.scaled.1 ) # ( estimate, age )
cor( d$hgt, d$age )

anstieg.2 * age.2.sd / hgt.2.sd 
tidy( lm.hgt.age.scaled.2 )
cor( d2$hgt, d2$age )

