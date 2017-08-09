##
# Gib Speicher frei
##
rm( list = ls( ) )

##
# intalliere HelperForLife-Paket, wenn noch nicht passiert
##
devtools::install_github( "TPeschel/hlpr4life" )

##
# lade hlpr4life
##
library( hlpr4life )

##
# installiere bei Bedarf und lade weiter Pakete
##
load.pkgs( c( "ggplot2", "broom", "reshape2", "lme4", "dplyr" ) )

##
# setze Startwert fuer Zufallsgenerator auf 1 wegen der Reproduzierbarkeit
##
#set.seed( 4 )




##
# Kreiere Tabelle mit Alter, den Gruppen Geschlecht und 
# Haarfarbe und der abhaengigen Variablen y
##

##
# Geschlechter
##
(
	sex.lvls <-
		c( "female", "male" ) )

##
# Haarfarben
##
(
	hair.lvls <-
		c( "black", "blonde", "brown" ) )

##
# die verschiedenen Gruppen als Kombination aus Gruppe Sex und Gruppe Haarfarbe
# levels: "female:black", "female:blonde", "female:brown", "male:black", "male:blonde", "male:brown"
##
( 
	prms <-
		data.frame(
			lvls = as.vector( sapply( sex.lvls, function( l ) paste0( l, ":", hair.lvls ) ) ), #levels
			mue  = m<-c( 30, 35, 32, 39, 41, 38 ), #mean
			var  = v<-c( 5, 3, 5, 9, 7, 10 ), #variance
			slp  = m + v ) ) # slope

##
# Farben fuer Plots in Abhaengigkeit vom Geschlecht und der Haarfarbe
##
( 
	cols.sex.hair <-
		c( "deeppink", "deeppink2", "deeppink4", "deepskyblue", "deepskyblue2", "deepskyblue4" ) )

##
# Farben fuer Plots in Abhaengigkeit vom Geschlecht
##
( 
	cols.sex <-
		c( "deeppink", "deepskyblue" ) )

##
# Farben fuer Plots in Abhaengigkeit von der Haarfarbe
##
( 
	cols.hair <-
		c( "#302020", "#d0a030", "#a88020" ) )

##
# Anzahl an Messungen
##
( 
	n <-
		10000 )

##
# Erstelle Tabelle Alter und Gruppen 
##
d <-
	data.frame(
		age  = runif( n, 3, 18 ),
		sex  = sample( sex.lvls, n, T ),
		hair = sample( hair.lvls, n, T ) )

##
# Berechne abhaengige Zufallswerte fuer Variable height = y
##
d$y <-
	1.5 * 
	( 50 + rnorm( 
		n, 
		prms$mue[ match( paste0( d$sex, ":", d$hair ), prms$lvls ) ],
		prms$var[ match( paste0( d$sex, ":", d$hair ), prms$lvls ) ] ) +
			( .75 - exp( 1 - ( d$age - 1 ) / 6 ) ) * prms$slp[ match( paste0( d$sex, ":", d$hair ), prms$lvls ) ] )


##
# Zeige die ersten sechs Zeilen
head( d )

#########################################################################

##
# Auswertung
##

##
# fitte nur Alter
##
(
	lm.d.y.age <-
		lm( y ~ age, d ) )

##
# Fasse zusammen
##
tidy( summary( lm.d.y.age ) )
# age ist signifikant, bleibt im Modell

##
# Zeige Plot!
##
ggplot( 
	d,
	aes( age, y ) ) +
	theme_bw( ) +
	geom_point( alpha = .1 ) +
	geom_smooth( method = "lm", col = "black" )

##
# Nimm das Geschlecht hinzu!
##
(
	lm.d.y.age.sex.1 <-
		lm( y ~ age + sex, d ) )

tidy( summary( lm.d.y.age.sex.1 ) )

lm.d.y.age.sex.1.line <-
	data.frame( 
		sex =  s<-rep( c( "female", "male" ), 3 ), 
		x = x<-c( 0, 0, 3, 3, 18, 18 ),
		y = c(
			lm.d.y.age.sex.1$coefficients[ 1 ],
			lm.d.y.age.sex.1$coefficients[ 1 ] + lm.d.y.age.sex.1$coefficients[ 3 ],
			lm.d.y.age.sex.1$coefficients[ 1 ] + lm.d.y.age.sex.1$coefficients[ 2 ] * 3,
			lm.d.y.age.sex.1$coefficients[ 1 ] + lm.d.y.age.sex.1$coefficients[ 3 ] + lm.d.y.age.sex.1$coefficients[ 2 ] * 3,
			lm.d.y.age.sex.1$coefficients[ 1 ] + lm.d.y.age.sex.1$coefficients[ 2 ] * 18,
			lm.d.y.age.sex.1$coefficients[ 1 ] + lm.d.y.age.sex.1$coefficients[ 3 ] + lm.d.y.age.sex.1$coefficients[ 2 ] * 18 ) )

ggplot( ) +
	theme_bw( ) +
	geom_point( aes( age, y, col = sex ), d, alpha = .1 ) +
	scale_color_manual( values = cols.sex, guide = F ) +
	geom_line( aes( x, y, col = sex ), lm.d.y.age.sex.1.line ) +
	xlim( 0, 18 )

(
	lm.d.y.age.sex.2 <-
		lm( y ~ age * sex, d ) )

tidy( summary( lm.d.y.age.sex.2 ) )

anova( lm.d.y.age.sex.1, lm.d.y.age.sex.2 )

# sex ist nicht signifikant
# aber age und age in abhaengigkeit von sex sind signifikant
# also zwei unterschiedliche Intercepts und Anstiege fuer die Geschlechter
ggplot( 
	d,
	aes( age, y, col = sex ) ) +
	theme_bw( ) +
	geom_point( alpha = .1 ) +
	geom_smooth( method = "lm" ) +
	scale_color_manual( values = cols.sex, guide = F )

##
# Vergleiche die beiden Modelle
# Der p-Wert gibt Aufschluss darueber, ob sich das eine Modell signifikant vom anderen unterscheidet und 
# ein kleinerer Wert fuer RSS gibt zeigt das bessere Modell an
##
anova( lm.d.y.age, lm.d.y.age.sex )
##
# hier ist das 2. besser
##


##
# Sex raus, Haare rein!
##
(
	lm.d.y.age.hair <-
		lm( y ~ age * hair, d ) )

summary( lm.d.y.age.hair )

ggplot( 
	d,
	aes( age, y, col = hair ) ) +
	theme_bw( ) +
	geom_point( alpha = .1 ) +
	geom_smooth( method = "lm" ) +
	scale_color_manual( values = cols.hair, guide = F )

##
# besser als das "Nur-Alter-Modell"
##
anova( lm.d.y.age, lm.d.y.age.hair )

##
# Haare erklaeren mehr als Sex
##
anova( lm.d.y.age.hair, lm.d.y.age.sex )

##
# jetzt mal alles rein, Alter, Geschlecht, Haare
##
(
	lm.d.y.age.sex.hair <-
		lm( y ~ age * sex * hair, d ) )

summary( lm.d.y.age.sex.hair )

anova( lm.d.y.age.sex.hair, lm.d.y.age.hair )
anova( lm.d.y.age.sex.hair, lm.d.y.age.sex )
##
# Das vollstaendige Modell ist das Beste
##

##
# diverse Plots um Sache zu veranschaulichen
##
ggplot( 
	d,
	aes( age, y, col = paste0( sex, ":", hair ) ) ) +
	theme_bw( ) +
	geom_point( alpha = .1 ) +
	geom_smooth( method = "lm" ) +
	scale_color_manual( values = cols.sex.hair, guide = F )

ggplot( 
	d,
	aes( age, y, col = paste0( sex, ":", hair ) ) ) +
	theme_bw( ) +
	geom_point( alpha = .1 ) +
	geom_smooth( method = "lm" ) +
	facet_grid( . ~ sex ) +
	scale_color_manual( values = cols.sex.hair, guide = F )

ggplot( 
	d,
	aes( age, y, col = paste0( sex, ":", hair ) ) ) +
	theme_bw( ) +
	geom_point( alpha = .1 ) +
	geom_smooth( method = "lm" ) +
	facet_grid( . ~ hair ) +
	scale_color_manual( values = cols.sex.hair, guide = F )

##
# model matrix
##
(
	m <-
		attr( lm.d.y.age.sex.hair$terms, "factors" ) )

##
# Regressionskoeffizienten
##
(
	reg.coef <-
		lm.d.y.age.sex.hair$coefficients )

##
# Echte Koeffizienten berechnen
##

##
# Alle Intercepts in den Gruppen
##
( y0_female_black  <- reg.coef[ "(Intercept)" ] )

( y0_female_blonde <- reg.coef[ "(Intercept)" ] + reg.coef[ "hairblonde" ] )
( y0_female_brown  <- reg.coef[ "(Intercept)" ] + reg.coef[ "hairbrown" ] )
( y0_male_black    <- reg.coef[ "(Intercept)" ] + reg.coef[ "sexmale" ] )

( y0_male_blonde   <- reg.coef[ "(Intercept)" ] + reg.coef[ "sexmale" ] + reg.coef[ "hairblonde" ] + reg.coef[ "sexmale:hairblonde" ] )
( y0_male_brown    <- reg.coef[ "(Intercept)" ] + reg.coef[ "sexmale" ] + reg.coef[ "hairbrown" ]  + reg.coef[ "sexmale:hairbrown" ] )

##
# Alle Anstiege in den Gruppen
##
( beta_female_black  <- reg.coef[ "age" ] )

( beta_female_blonde <- reg.coef[ "age" ] + reg.coef[ "age:hairblonde" ] )
( beta_female_brown  <- reg.coef[ "age" ] + reg.coef[ "age:hairbrown" ] )
( beta_male_black    <- reg.coef[ "age" ] + reg.coef[ "age:sexmale" ] )

( beta_male_blonde   <- reg.coef[ "age" ] + reg.coef[ "age:sexmale" ] + reg.coef[ "age:hairblonde" ] + reg.coef[ "age:sexmale:hairblonde" ] )
( beta_male_brown    <- reg.coef[ "age" ] + reg.coef[ "age:sexmale" ] + reg.coef[ "age:hairbrown" ]  + reg.coef[ "age:sexmale:hairbrown" ] )

##
# Plotte alle Gruppen
##
(
	plt <-
		ggplot( 
			d,
			aes( age, y, col = paste0( sex, ":", hair ) ) ) +
		theme_bw( ) +
		geom_point( alpha = .1 ) +
		geom_smooth( method = "lm" ) +
		facet_grid( sex ~ hair ) +
		scale_color_manual( values = cols.sex.hair, guide = F ) )

##
# Berechne aus "echten" Koeffizienten die y-Werte bei 3 und 18 Jahren 
## 
(
	fit <-
		data.frame(
			sex = c(
				rep( sex.lvls[ 1 ], 3 ),
				rep( sex.lvls[ 2 ], 3 ) ),
			hair = rep( hair.lvls, 2 ),
			y0 = c(
				y0_female_black,
				y0_female_blonde,
				y0_female_brown,
				y0_male_black,
				y0_male_blonde,
				y0_male_brown ),
			beta = c(
				beta_female_black,
				beta_female_blonde,
				beta_female_brown,
				beta_male_black,
				beta_male_blonde,
				beta_male_brown ) ) )

fit$xs <-
	3

fit$ys <-
	fit$y0 + fit$beta * fit$xs

fit$xf <-
	18

fit$yf <-
	fit$y0 + fit$beta * fit$xf

##
# Erzeuge Dataframe fuer Anfangs- und Endpunkte (3,18)
##
(
	lmts <-
		data.frame(
			age           = a <- c( 3, 18 ),
			female_black  = y0_female_black  + beta_female_black * a,
			female_blonde = y0_female_blonde + beta_female_blonde * a,
			female_brown  = y0_female_brown  + beta_female_brown * a,
			male_black    = y0_male_black    + beta_male_black * a,
			male_blonde   = y0_male_blonde   + beta_male_blonde * a,
			male_brown    = y0_male_brown    + beta_male_brown * a ) )

##
# Fuege letztem Plot, gefittete Anfangs und Endpunkte hinzu
##
plt + 
	geom_point( aes( xs, ys, col = paste0( fit$sex, ":", fit$hair ) ), fit, shape = 1, size = 3 ) +
	geom_point( aes( xf, yf, col = paste0( fit$sex, ":", fit$hair ) ), fit, shape = 2, size = 3 )

##
# Uebersicht uber Regression
##
tidy( lm.d.y.age.sex.hair )

##
# Ubersicht ueber tatsaechliche Schnittpunkt und Anstiege
##
fit[ , c( 1 : 4 ) ]
##
# Vergleiche dazu Simulationsparameter
##
prms
##
# mue = y0
# beta = slp    ... slope
##

(
	fit$lvls <-
		paste0( fit$sex, ":", fit$hair ) )

(
	fit <-
		fit[ , c( "lvls", "y0", "beta" ) ] )

(
	cmpr <-
		merge(
			fit,
			prms,
			by = "lvls" )[ , c( 1, 2, 4, 3, 6, 5 ) ] )

(
	cmpr$dy <-
		cmpr$y0 - cmpr$mue )

(
	cmpr$dbeta <-
		cmpr$beta - cmpr$slp )

(
	cmpr <-
		cmpr[ , c( 1, 2, 3, 7, 4, 5, 8, 6 ) ] )

(
	cmpr <-
		rbind(
			cmpr,
			c( sum = "sum", sapply( cmpr[ , -1 ], sum ) ) ) )

( cmpr <-
		cbind( data.frame( lvls = cmpr$lvls ), as.data.frame( lapply( cmpr[ , -1 ], as.numeric ) ) ) )

arrange( cmpr, var )


