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
set.seed( 4 )

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
			var  = v<-c( 7, 5, 7, 11, 13, 15 ), #variance
			slp  = .16 * ( m + v ) ) ) # slope

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
		1000 )

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
	rnorm( 
		n, 
		prms$mue[ match( paste0( d$sex, ":", d$hair ), prms$lvls ) ],
		prms$var[ match( paste0( d$sex, ":", d$hair ), prms$lvls ) ] ) +
		d$age * prms$slp[ match( paste0( d$sex, ":", d$hair ), prms$lvls ) ]


##
# Zeige die ersten sechs Zeilen
head( d )
	
#########################################################################

##
# Auswertung
##

##
# Zeige Plot!
##
ggplot( ) +
	geom_point( aes( age, y ), d ) +
	theme_bw( )

##
# ganz offensichtlich gibt es ein Abhaengikeit vom Alter
# also sollte in jedem Modell das Alter eine Rolle spielen
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

co <- 
	lm.d.y.age$coefficients

lm.d.y.age.line <-
	data.frame( 
		x = age<-c( 0, 3, 18 ),
		y = co[ "(Intercept)" ] + 
			co[ "age" ] * age )
##
# Zeige Plot!
##
ggplot( ) +
	theme_bw( ) + xlim( 0, 20 ) + ylim( 0, 250 ) +
	geom_point( aes( age, y ), d ) +
	geom_smooth( aes( age, y ), d, method = "lm", col = "gray" ) +
	geom_line( aes( x, y ), lm.d.y.age.line, col = "gray" ) +
	geom_point( aes( x, y, shape = as.factor( x ) ), lm.d.y.age.line, size = 4, col = "gray" ) +
	geom_text( aes( x, y, label = round( y, 2 ) ), lm.d.y.age.line, nudge_x = 0, nudge_y = -1, col = "black" )
	

##
# Nimm das Geschlecht hinzu!
##
(
	lm.d.y.age.sex.1 <-
		lm( y ~ age + sex, d ) )

tidy( summary( lm.d.y.age.sex.1 ) )

co <-
	lm.d.y.age.sex.1$coefficients
	
lm.d.y.age.sex.1.line <-
	data.frame( 
		sex =  s<-rep( c( "female", "male" ), 3 ), 
		x = age<-c( 0, 0, 3, 3, 18, 18 ),
		sex.lvl <- c( 0, 1 )[ match( s, sex.lvls ) ],
		y = co[ "(Intercept)" ] + sex.lvl * co[ "sexmale" ] + co[ "age" ] * age )

ggplot( ) +
	theme_bw( ) + xlim( 0, 20 ) + ylim( 0, 250 ) +
	geom_point( aes( age, y, col = sex ), d, alpha = .1 ) +
#	geom_smooth( aes( age, y, col = sex ), d, method = "lm", alpha = .1 ) +
	scale_color_manual( values = cols.sex, guide = F ) +
	geom_line( aes( x, y, col = sex ), lm.d.y.age.sex.1.line ) +
	geom_point( aes( x, y, shape = as.factor( x ), col = sex ), lm.d.y.age.sex.1.line, size = 3 ) +
	geom_text( aes( x, y, label = round( y, 2 ), col = sex ), lm.d.y.age.sex.1.line, nudge_x = 0, nudge_y = -5 )

(
	lm.d.y.age.sex.2 <-
		lm( y ~ age : sex, d ) )

tidy( summary( lm.d.y.age.sex.2 ) )

co <-
	lm.d.y.age.sex.2$coefficients

lm.d.y.age.sex.2.line <-
	data.frame( 
		sex =  s<-rep( c( "female", "male" ), 3 ), 
		x = age<-c( 0, 0, 3, 3, 18, 18 ),
		y = co[ "(Intercept)" ] + co[ paste0( "age:sex", s ) ] * age )

ggplot( ) +
	theme_bw( ) + xlim( 0, 20 ) + ylim( 0, 250 ) +
	geom_point( aes( age, y, col = sex ), d, alpha = .1 ) +
#	geom_smooth( aes( age, y, col = sex ), d, method = "lm", alpha = .1 ) +
	scale_color_manual( values = cols.sex, guide = F ) +
	geom_line( aes( x, y, col = sex ), lm.d.y.age.sex.2.line ) +
	geom_point( aes( x, y, shape = as.factor( x ), col = sex ), lm.d.y.age.sex.2.line, size = 3 ) +
	geom_text( aes( x, y, label = round( y, 2 ), col = sex ), lm.d.y.age.sex.2.line, nudge_x = 0, nudge_y = -5 )

(
	lm.d.y.age.sex.3 <-
		lm( y ~ age * sex, d ) )

tidy( summary( lm.d.y.age.sex.3 ) )

(
	co <-
		lm.d.y.age.sex.3$coefficients )

lm.d.y.age.sex.3.line <-
	data.frame( 
		sex =  s<-rep( sex.lvls, 3 ), 
		x = age<-c( 0, 0, 3, 3, 18, 18 ),
		sex.male <- c( 0, 1 )[ match( s, sex.lvls ) ],
		y = co[ "(Intercept)" ] + sex.male * co[ "sexmale" ] + 
			( co[ "age" ]  + sex.male * co[ "age:sexmale" ] ) * age )

ggplot( ) +
	theme_bw( ) + xlim( 0, 20 ) + ylim( 0, 250 ) +
	geom_point( aes( age, y, col = sex ), d, alpha = .1 ) +
#	geom_smooth( aes( age, y, col = sex ), d, method = "lm", alpha = .1 ) +
	scale_color_manual( values = cols.sex, guide = F ) +
	geom_line( aes( x, y, col = sex ), lm.d.y.age.sex.3.line ) +
	geom_point( aes( x, y, shape = as.factor( x ), col = sex ), lm.d.y.age.sex.3.line, size = 3 ) +
	geom_text( aes( x, y, label = round( y, 2 ), col = sex ), lm.d.y.age.sex.3.line, nudge_x = 0, nudge_y = -5 )

anova( lm.d.y.age, lm.d.y.age.sex.1, lm.d.y.age.sex.2, lm.d.y.age.sex.3 )
# das letzte modell ist das beste
# kleinster RSS-Wert

##
# also hier gehts weiter
##
lm.d.y.age.sex <- 
	lm.d.y.age.sex.3

##
# jetzt Haare rein!
##
(
	lm.d.y.age.sex.hair.1 <-
		lm( y ~ age * sex + hair, d ) )

tidy( summary( lm.d.y.age.sex.hair.1 ) )

co <-
	lm.d.y.age.sex.hair.1$coefficients

##
# model matrix
##
mm <-
	(
		data.frame(
			'(Intercept)' = 1,
			age =
				c( 0, 0, 0, 0, 0, 0, 3, 3, 3, 3, 3, 3, 18, 18, 18, 18, 18, 18 ),
			sexmale =
				c( 0, 0, 0, 1, 1, 1, 0, 0, 0, 1, 1, 1,  0,  0,  0,  1,  1,  1 ),
			hairblonde =
				c( 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0,  0,  1,  0,  0,  1,  0 ),
			hairbrown =
				c( 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1,  0,  0,  1,  0,  0,  1  ),
			'age:sexmale' =
				c( 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 3, 3,  0,  0,  0,  18, 18, 18 ) ) )

lm.d.y.age.sex.hair.1.line <-
	data.frame( 
		sex  =  rep( c( rep( "female", 3 ), rep( "male", 3 ) ), 3 ),
		hair = rep( hair.lvls, 2 * 3 ), 
		x    = mm$age,
		y    = as.matrix( mm ) %*% co )

ggplot( ) +
	theme_bw( ) + xlim( 0, 20 ) + ylim( 0, 250 ) +
	scale_color_manual( values = cols.sex.hair, guide = F ) +
	geom_point( aes( age, y, col = paste0( sex, ":", hair ) ), d, alpha = .1 ) +
	geom_smooth( aes( age, y, col = paste0( sex, ":", hair ) ), d, method = "lm" ) +
	geom_line( aes( x, y, col = paste0( sex, ":", hair ) ), lm.d.y.age.sex.hair.1.line ) +
	geom_point( aes( x, y, shape = as.factor( x ), col = paste0( sex, ":", hair ) ), lm.d.y.age.sex.hair.1.line, size = 3 ) +
	geom_text( aes( x, y, label = round( y, 2 ), col = paste0( sex, ":", hair ) ), lm.d.y.age.sex.hair.1.line, nudge_x = 0, nudge_y = -5 ) +
	facet_grid( sex ~ hair )

(
	lm.d.y.age.sex.hair.2 <-
		lm( y ~ age * sex + age : hair, d ) )

tidy( summary( lm.d.y.age.sex.hair.2 ) )

mm <-
	(
		data.frame(
			'(Intercept)' = 1,
			age =
				c( 0, 0, 0, 0, 0, 0, 3, 3, 3, 3, 3, 3, 18, 18, 18, 18, 18, 18 ),
			sexmale =
				c( 0, 0, 0, 1, 1, 1, 0, 0, 0, 1, 1, 1,  0,  0,  0,  1,  1,  1 ),
			'age:sexmale' =
				c( 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 3, 3,  0,  0,  0, 18, 18, 18 ),
			'age:hairblonde' =
				c( 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 3, 0,  0, 18,  0,  0, 18,  0 ),
			"age:hairbrown" =
				c( 0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 3,  0,  0, 18,  0,  0, 18  ) ) )

co <-
	lm.d.y.age.sex.hair.2$coefficients

lm.d.y.age.sex.hair.2.line <-
	data.frame( 
		sex = sex.lvls[ match( mm$sexmale, c( 0, 1 ) ) ],
		hair = rep( hair.lvls, 6 ), 
		x =  mm$age,
		y = as.matrix( mm ) %*% co )

ggplot( ) +
	theme_bw( ) + xlim( 0, 20 ) + ylim( 0, 250 ) +
	scale_color_manual( values = cols.sex.hair, guide = F ) +
	geom_point( aes( age, y, col = paste0( sex, ":", hair ) ), d, alpha = .1 ) +
	geom_smooth( aes( age, y, col = paste0( sex, ":", hair ) ), d, method = "lm" ) +
	geom_line( aes( x, y, col = paste0( sex, ":", hair ) ), lm.d.y.age.sex.hair.2.line ) +
	geom_point( aes( x, y, shape = as.factor( x ), col = paste0( sex, ":", hair ) ), lm.d.y.age.sex.hair.2.line, size = 3 ) +
	geom_text( aes( x, y, label = round( y, 2 ), col = paste0( sex, ":", hair ) ), lm.d.y.age.sex.hair.2.line, nudge_x = 0, nudge_y = -5 ) +
	facet_grid( hair ~ sex )

(
	lm.d.y.age.sex.hair.3 <-
		lm( y ~ age * ( sex + hair ), d ) )

tidy( summary( lm.d.y.age.sex.hair.3 ) )

mm <-
	(
		data.frame(
			'(Intercept)' = 1,
			age =
				c( 0, 0, 0, 0, 0, 0, 3, 3, 3, 3, 3, 3, 18, 18, 18, 18, 18, 18 ),
			sexmale =
				c( 0, 0, 0, 1, 1, 1, 0, 0, 0, 1, 1, 1,  0,  0,  0,  1,  1,  1 ),
			'hairblonde' =
				c( 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0,  0,  1,  0,  0,  1,  0 ),
			"hairbrown" =
				c( 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1,  0,  0,  1,  0,  0,  1  ),
			'age:sexmale' =
				c( 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 3, 3,  0,  0,  0, 18, 18, 18 ),
			'age:hairblonde' =
				c( 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 3, 0,  0, 18,  0,  0, 18,  0 ),
			"age:hairbrown" =
				c( 0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 3,  0,  0, 18,  0,  0, 18  ) ) )

co <-
	lm.d.y.age.sex.hair.3$coefficients

lm.d.y.age.sex.hair.3.line <-
	data.frame( 
		sex = sex.lvls[ match( mm$sexmale, c( 0, 1 ) ) ],
		hair = rep( hair.lvls, 6 ), 
		x =  mm$age,
		y = as.matrix( mm ) %*% co )

ggplot( ) +
	theme_bw( ) + xlim( -1, 20 ) + ylim( 0, 250 ) +
	scale_color_manual( values = cols.sex.hair, guide = F ) +
	geom_point( aes( age, y, col = paste0( sex, ":", hair ) ), d, alpha = .1 ) +
	geom_smooth( aes( age, y, col = paste0( sex, ":", hair ) ), d, method = "lm" ) +
	geom_line( aes( x, y, col = paste0( sex, ":", hair ) ), lm.d.y.age.sex.hair.3.line ) +
	geom_point( aes( x, y, shape = as.factor( x ), col = paste0( sex, ":", hair ) ), lm.d.y.age.sex.hair.3.line, size = 3 ) +
	geom_text( aes( x, y, label = round( y, 2 ), col = paste0( sex, ":", hair ) ), lm.d.y.age.sex.hair.3.line, nudge_x = 0, nudge_y = -5 ) +
	facet_grid( hair ~ sex )


# jetzt wieder alle Modelle mithilfe von anova vergleichen
anova( lm.d.y.age.sex.hair.1, lm.d.y.age.sex.hair.2, lm.d.y.age.sex.hair.3 )

##
# Das 3. Modell ist wieder das beste und das engueltige
##

# Ergebnisse
tidy( lm.d.y.age.sex.hair.3 )

# urspruengliche Simulationsparameter
prms


##
# ein paar Beispiele zum Nachvollziehen, der Koeffizientenberechnung
##



##
# black female at age of 0
##
lm.d.y.age.sex.hair.3$coefficients[ "(Intercept)" ]

##
# theoretische Wert bei
##
prms$mue[ prms$lvls == "female:black" ]




##
# black female at age of 10
##
lm.d.y.age.sex.hair.3$coefficients[ "(Intercept)" ] + lm.d.y.age.sex.hair.3$coefficients[ "age" ] * 10 

##
# theoretische Wert bei
##
prms$mue[ prms$lvls == "female:black" ] + prms$slp[ prms$lvls == "female:black" ] * 10



##
# blonde male at age of 0  ( Intercept )
##
lm.d.y.age.sex.hair.3$coefficients[ "(Intercept)" ] + lm.d.y.age.sex.hair.3$coefficients[ "sexmale" ] + lm.d.y.age.sex.hair.3$coefficients[ "hairblonde" ]

##
# theoretische Wert bei
##
prms$mue[ prms$lvls == "male:blonde" ]



##
# brown female at age of 3
##
lm.d.y.age.sex.hair.3$coefficients[ "(Intercept)" ] + lm.d.y.age.sex.hair.3$coefficients[ "hairbrown" ] + 
	( lm.d.y.age.sex.hair.3$coefficients["age"] + lm.d.y.age.sex.hair.3$coefficients["age:hairbrown"] ) * 3

##
# theoretische Wert bei
##
prms$mue[ prms$lvls == "female:brown" ] + prms$slp[ prms$lvls == "female:brown" ] * 3 

