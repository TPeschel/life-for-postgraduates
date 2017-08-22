rm( list = ls( ) )

if( !"devtools" %in% rownames( installed.packages( ) ) ) install.packages( "devtools" )

#devtools::install_github( "TPeschel/hlpr4life" )

hlpr4life::load.pkgs( c( "hlpr4life", "ggplot2", "ggthemes", "broom" ) )

###########################################################

##
# definiere ein paar plots fuer spaeter
# macht alles bequehmer
##

set.seed( 1 )

###########################################################

##
# Unsere Tabelle enthaelt immer N = 1000 Messungen
##
N <-
	1000

#################################################################

x1 <-
	rnorm( N, +10 )

x2 <-
	rnorm( N, +10 )

y <-
	x1 - x2 + zeit

d <-
	data.frame( zeit, y, x1, x2 )

ggsubplot(
	ggplot( d, aes( x1, x2 ) ) + theme_bw( ) + geom_point( ) + geom_smooth( method = "lm" ),
	ggplot( d, aes( x1, y ) ) + theme_bw( ) + geom_point( ) + geom_smooth( method = "lm" ),
	ggplot( ) + theme_solid( fill = "white" ),
	ggplot( d, aes( x2, y ) ) + theme_bw( ) + geom_point( ) + geom_smooth( method = "lm" ),
	cols = 2 )
	
tidy(
	lm.y.x1.x2 <-
		lm( y ~ x1 + x2, d ) )

cor( d$x1, d$x2 )
cor( d$x1, d$y )
cor( d$x2, d$y )

d.scaled <-
	as.data.frame( sapply( d, scale ) )

tidy(
	lm.scaled.y.x1.x2 <-
		lm( y ~ x1 + x2, d.scaled ) )

cor( d.scaled$x1, d.scaled$x2 )
cor( d.scaled$x1, d.scaled$y )
cor( d.scaled$x2, d.scaled$y )


zeit <-
	runif( N, 0, 20 )

x1 <-
	zeit + rnorm( N, +1 )

x2 <-
	zeit + rnorm( N, +1 )

y <-
	x1 - x2

d <-
	data.frame( zeit, y, x1, x2 )

ggsubplot(
	ggplot( d, aes( x1, x2, col = y ) ) + theme_bw( ) + geom_point( ) + geom_smooth( method = "lm" ),
	ggplot( d, aes( x1, y, col = x2 ) ) + theme_bw( ) + geom_point( ) + geom_smooth( method = "lm" ),
	ggplot( ) + theme_solid( fill = "white" ),
	ggplot( d, aes( x2, y, col = x1 ) ) + theme_bw( ) + geom_point( ) + geom_smooth( method = "lm" ),
	cols = 2 )

tidy(
	lm.y.x1.x2 <-
		lm( y ~ x1 + x2, d ) )

cor( d$x1, d$x2 )
cor( d$x1, d$y )
cor( d$x2, d$y )

d.scaled <-
	as.data.frame( sapply( d, scale ) )

tidy(
	lm.scaled.y.x1.x2 <-
		lm( y ~ x1 + x2, d.scaled ) )

cor( d.scaled$x1, d.scaled$x2 )
cor( d.scaled$x1, d.scaled$y )
cor( d.scaled$x2, d.scaled$y )

ggsubplot(
	ggplot( d.scaled, aes( x1, x2, col = y ) ) + theme_bw( ) + geom_point( ) + geom_smooth( method = "lm" ),
	ggplot( d.scaled, aes( x1, y, col = x2 ) ) + theme_bw( ) + geom_point( ) + geom_smooth( method = "lm" ),
	ggplot( d.scaled, aes( zeit, y, col = x1 - x2 ) ) + theme_bw( ) + geom_point( ) + geom_smooth( method = "lm" ),
	ggplot( d.scaled, aes( x2, y, col = x1 ) ) + theme_bw( ) + geom_point( ) + geom_smooth( method = "lm" ),
	cols = 2 )

