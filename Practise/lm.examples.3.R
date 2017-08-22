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
load.pkgs( c( "ggplot2", "broom", "reshape2", "lme4", "dplyr", "latex2exp" ) )

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
			slp  = m + v ) ) # slope

##
# Farben fuer Plots in Abhaengigkeit vom Geschlecht und der Haarfarbe
##
( 
	cols.sex.hair <-
		c( "black", "yellow", "brown", "midnightblue", "orange", "orange4" ) )

##
# Anzahl an Messungen
##
( 
	n <-
		1500 )

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
	.16 * d$age * prms$slp[ match( paste0( d$sex, ":", d$hair ), prms$lvls ) ]

modelMatrix <-
	function( formula, ages )
		model.matrix(
			lm( formula = formula,
				merge(
					merge(
						data.frame( y = 1 ),
						data.frame( age = ages ) ),
					merge(
						data.frame( sex = sex.lvls ),
						data.frame( hair = hair.lvls ) ) ) ) )

plot.lin.reg <-
	function( frml, data, txt = "", verbose = T ) {
	
	# (
	# 	frml <-
	# 		"y ~ hair * sex" )
	
	ages <-
		c( 0, 3, 18  )
	
	mm <-
		modelMatrix( frml, ages )
	

	m <-
		as.matrix( mm )
	

	lm.d <-
		lm( frml, data )
	
	co <-
		lm.d$coefficients
	
	m %*% co
	
	lm.d.line <-
		function( mm ) {
			cmm <-
				as.data.frame( 
					modelMatrix( 
						y ~ sex * hair * age, 
						c( 0, 3, 18 ) ) )
			data.frame( 
				sex = sex.lvls[ match( cmm$sexmale, c( 0, 1 ) ) ],
				hair = hair.lvls[ match( cmm$hairblonde + 2 * cmm$hairbrown, c( 0, 1, 2 ) ) ],
				x = cmm$age,
				y = as.matrix( mm ) %*% co ) }
	
	line <-
		lm.d.line( mm )

	p <-
		ggplot( )+ gitlab
			theme_bw( ) + xlim( -1, 20 ) + ylim( 0, 230 ) + ggtitle( frml, txt ) +
			scale_color_manual( values = cols.sex.hair, guide = F ) +
			scale_shape_discrete( guide = F ) +
			geom_point( aes( age, y, col = paste0( sex, ":", hair ) ), data, alpha = .1 ) +
	#		geom_smooth( aes( age, y, col = paste0( sex, ":", hair ) ), data, method = "lm", alpha = .01 ) +
			geom_line( aes( x, y, col = paste0( sex, ":", hair ) ), line ) +
			geom_point( aes( x, y, shape = as.factor( x ), col = paste0( sex, ":", hair ) ), line, size = 3 )
	if( verbose )
		p <- p + geom_text( aes( x, y, label = round( y, 2 ) ), line, col = "black" )
	p }


plot.lin.reg( "y ~ 0", d, "empty model no intercept no slope" )
plot.lin.reg( "y ~ 1", d, "null model only 1 intercept (overall mean)"  )
plot.lin.reg( "y ~ sex", d, "only 2 different intercepts for sex\n(group means for sex)"  )
plot.lin.reg( "y ~ hair", d, "only 3 different intercepts for hair\n(group means for hair)"  )
plot.lin.reg( "y ~ sex + hair", d, "6 different but dependent intercepts for sex and hair\n(group means for sex and hair)"  )
plot.lin.reg( "y ~ sex * hair", d, "6 different independent intercepts for sex and hair\n(group means for sex and hair)"  )
plot.lin.reg( "y ~ age", d, "only 1 slope for all groups" )
plot.lin.reg( "y ~ age + sex", d, "only 1 slope for all groups\nbut 2 different intercepts for sex" )
plot.lin.reg( "y ~ age + hair", d, "only 1 slope for all groups\nbut 3 different intercepts for hair" )
plot.lin.reg( "y ~ age + sex + hair", d, "only 1 slope for all groups\nbut 6 different intercepts for sex and hair" )
plot.lin.reg( "y ~ age * sex + hair", d, "2 different slopes for sex and\n6 different intercepts for sex and hair" )
plot.lin.reg( "y ~ age * hair + sex", d, "3 different slopes for hair and\n6 different intercepts for sex and hair" )
plot.lin.reg( "y ~ age : ( hair + sex )", d, "only 1 intercept and \n6 different slopes for all groups" )
plot.lin.reg( "y ~ age * ( hair + sex )", d, "6 different intercepts and \n6 different but dependent slopes for all groups" )
plot.lin.reg( "y ~ age * hair * sex", d, "complete model" )

plot.lin.reg( "y ~ 0", d, "empty model no intercept no slope"  ) + facet_grid( sex ~ hair )
plot.lin.reg( "y ~ 1", d, "null model only 1 intercept (overall mean)"  ) + facet_grid( sex ~ hair )
plot.lin.reg( "y ~ sex", d, "only 2 different intercepts for sex\n(group means for sex)"  ) + facet_grid( sex ~ hair )
plot.lin.reg( "y ~ hair", d, "only 3 different intercepts for hair\n(group means for hair)"  ) + facet_grid( sex ~ hair )
plot.lin.reg( "y ~ sex + hair", d, "6 different but dependent intercepts for sex and hair\n(group means for sex and hair)"  ) + facet_grid( sex ~ hair)
plot.lin.reg( "y ~ sex * hair", d, "6 different independent intercepts for sex and hair\n(group means for sex and hair)"  ) + facet_grid( sex ~ hair)
plot.lin.reg( "y ~ age", d, "only 1 slope for all groups" ) + facet_grid( sex ~ hair)
plot.lin.reg( "y ~ age + sex", d, "only 1 slope for all groups\nbut 2 different intercepts for sex" ) + facet_grid( sex ~ hair)
plot.lin.reg( "y ~ age + hair", d, "only 1 slope for all groups\nbut 3 different intercepts for hair" ) + facet_grid( sex ~ hair)
plot.lin.reg( "y ~ age + sex + hair", d, "only 1 slope for all groups\nbut 6 different intercepts for sex and hair" ) + facet_grid( sex ~ hair)
plot.lin.reg( "y ~ age * sex + hair", d, "2 different slopes for sex and\n6 different intercepts for sex and hair" ) + facet_grid( sex ~ hair)
plot.lin.reg( "y ~ age * hair + sex", d, "3 different slopes for hair and\n6 different intercepts for sex and hair" ) + facet_grid( sex ~ hair)
plot.lin.reg( "y ~ age : ( hair + sex )", d, "only 1 intercept and \n6 different slopes for all groups" ) + facet_grid( sex ~ hair)
plot.lin.reg( "y ~ age * ( hair + sex )", d, "6 different intercepts and \n6 different but dependent slopes for all groups" ) + facet_grid( sex ~ hair)
plot.lin.reg( "y ~ age * hair * sex", d, "complete model" ) + facet_grid( sex ~ hair)

ggsubplot(
	plot.lin.reg( "y ~ 0", d, "empty model no intercept no slope", F  ),
	plot.lin.reg( "y ~ 1", d, "null model only 1 intercept (overall mean)", F  ),
	plot.lin.reg( "y ~ sex", d, "only 2 different intercepts for sex\n(group means for sex)", F  ),
	plot.lin.reg( "y ~ hair", d, "only 3 different intercepts for hair\n(group means for hair)", F  ),
	plot.lin.reg( "y ~ sex + hair", d, "6 different but dependent intercepts for sex and hair\n(group means for sex and hair)", F  ),
	plot.lin.reg( "y ~ sex * hair", d, "6 different independent intercepts for sex and hair\n(group means for sex and hair)", F  ),
	plot.lin.reg( "y ~ age", d, "only 1 slope for all groups", F ),
	plot.lin.reg( "y ~ age + sex", d, "only 1 slope for all groups\nbut 2 different intercepts for sex", F ),
	plot.lin.reg( "y ~ age + hair", d, "only 1 slope for all groups\nbut 3 different intercepts for hair", F ),
	plot.lin.reg( "y ~ age + sex + hair", d, "only 1 slope for all groups\nbut 6 different intercepts for sex and hair", F ),
	plot.lin.reg( "y ~ age * sex + hair", d, "2 different slopes for sex and\n6 different intercepts for sex and hair", F ),
	plot.lin.reg( "y ~ age * hair + sex", d, "3 different slopes for hair and\n6 different intercepts for sex and hair", F ),
	plot.lin.reg( "y ~ age : ( hair + sex )", d, "only 1 intercept and \n6 different but dependent slopes for all groups", F ),
	plot.lin.reg( "y ~ age * ( hair + sex )", d, "6 different intercepts and \n6 different but dependent slopes for all groups", F ),
	plot.lin.reg( "y ~ age * hair * sex", d, "complete model", F ),
	layout = 
		t(
			matrix( 
				c( 
					1, 2, 3, 4, 5,
					6, 7, 8, 9, 10, 
					11, 12, 13, 14, 15 ),
				nrow = 5 ) ) )

ggsubplot(
	plot.lin.reg( "y ~ 0", d, "empty model no intercept no slope"  ) + facet_grid( sex ~ hair ),
	plot.lin.reg( "y ~ 1", d, "null model only 1 intercept (overall mean)"  ) + facet_grid( sex ~ hair ),
	plot.lin.reg( "y ~ sex", d, "only 2 different intercepts for sex\n(group means for sex)"  ) + facet_grid( sex ~ hair ),
	plot.lin.reg( "y ~ hair", d, "only 3 different intercepts for hair\n(group means for hair)"  ) + facet_grid( sex ~ hair ),
	plot.lin.reg( "y ~ sex + hair", d, "6 different but dependent intercepts for sex and hair\n(group means for sex and hair)"  ) + facet_grid( sex ~ hair ),
	plot.lin.reg( "y ~ sex * hair", d, "6 different independent intercepts for sex and hair\n(group means for sex and hair)"  ) + facet_grid( sex ~ hair ),
	plot.lin.reg( "y ~ age", d, "only 1 slope for all groups" ) + facet_grid( sex ~ hair ),
	plot.lin.reg( "y ~ age + sex", d, "only 1 slope for all groups\nbut 2 different intercepts for sex" ) + facet_grid( sex ~ hair ),
	plot.lin.reg( "y ~ age + hair", d, "only 1 slope for all groups\nbut 3 different intercepts for hair" ) + facet_grid( sex ~ hair ),
	plot.lin.reg( "y ~ age + sex + hair", d, "only 1 slope for all groups\nbut 6 different intercepts for sex and hair" ) + facet_grid( sex ~ hair ),
	plot.lin.reg( "y ~ age * sex + hair", d, "2 different slopes for sex and\n6 different intercepts for sex and hair" ) + facet_grid( sex ~ hair ),
	plot.lin.reg( "y ~ age * hair + sex", d, "3 different slopes for hair and\n6 different intercepts for sex and hair" ) + facet_grid( sex ~ hair ),
	plot.lin.reg( "y ~ age : ( hair + sex )", d, "only 1 intercept and \n6 different but dependent slopes for all groups" ) + facet_grid( sex ~ hair ),
	plot.lin.reg( "y ~ age * ( hair + sex )", d, "6 different intercepts and \n6 different but dependent slopes for all groups" ) + facet_grid( sex ~ hair ),
	plot.lin.reg( "y ~ age * hair * sex", d, "complete model" ) + facet_grid( sex ~ hair ),
	layout = 
		t(
			matrix( 
				c( 
					1, 2, 3, 4, 5,
					6, 7, 8, 9, 10, 
					11, 12, 13, 14, 15 ),
				nrow = 5 ) ) )
