rm( list = ls( ) )

# install.packages( "devtools" )
# 
# devtools::install_github( "TPeschel/hlpr4life" )

library( hlpr4life )

Sys.time( )
Sys.timezone( )
Sys.setenv( TZ = "BMT" )

load.pkgs( 
	c( 
		"dplyr",
		"gamlss",
		"gamlss.data",
		"ggplot2",
		"ggthemes",
		"reshape2",
		"lubridate" ) )

from <-
	as_datetime( "2000-01-01" )

to <-
	as_datetime( "2017-12-31" )

res <-
	as.numeric( round( difftime( to, from ) / ( 365.25 / 12 ) ) )

n <-
	10000

d <-
	data.frame(
		date = seq( from = from, to = to, length.out = n ),
		y = rnorm( n ) + 
			( 2 * cos( .001 * c( 1 : n ) ) ) ** 2 * cos( .0001 * c( 1 : n ) ) ) + ( .0005 * c( 1 : n - n / 2 ) ) ** 2

( plot1 <-
	ggplot( d ) +
		geom_point( aes( date, y ), alpha = .1 ) +
		theme_bw( ) )

arrange( d, date )

summary( d$date )
# 
# d$date.month <-
# 	cut(
# 		d$date,
# 		breaks = 200 )

quntls <-
	function( df, col.date = "date", breaks = nrow( df ) / 100, probs = c( 0, .001, .01, .025, .05, .1, .25, .5, .75, .9, .95, .975, .99, .999, 1 ) ) {

		df$date.month <-
			cut(
				df[ , names( df ) == col.date ],
				breaks = breaks )
		
		txt <-
			paste0(
				'ss <- df %>% group_by( date.month ) %>% summarise(',
				paste( paste0( "p", 100 * probs[ 1 ] ), "=", paste0( "quantile(y,", probs[ 1 ], ")" ) ) )
		
		for( p in probs ) {
			
			txt <-
				paste0( txt, ",", paste( paste0( "p", 100 * p ),"=", paste0( "quantile(y,", p, ")" ) ) ) }
		txt <-
			paste0( txt, ")" )
		
		eval( parse( text = txt ) ) }

( plot2 <-
	ggplot( ) +
		geom_line( data = melt( quntls( d, "date", res ), "date.month" ), aes( date.month, value, group = variable, col = variable ), alpha = 1 ) +
		geom_point( data = d, aes( as.factor( date ), y ), col = "gray", alpha = .1 ) +
		theme_classic( ) )

brks <-
	res

perc <-
	quntls(
		df = d[ sample( c( T, F ), nrow( d ), T, prob = c( .7, .3 ) ), ],
		col.date = "date", 
		breaks = brks )

perc[ , -1 ] <-
	0

iters <-
	1

sub.iters <- 
	c( 3 : 3 )

p <-
	1

for( it in c( 1 : iters ) ) {

	print( paste( "iteration: ", it ) )
	
	smpl <-
		d[ sample( c( T, F ), nrow( d ), T, prob = c( p, ( 1 - p ) ) ), ]
	
	g <-
		quntls( smpl, "date", brks )

	for( b in sub.iters ) {
		
		sm <-
			exp( -( c( -b : b ) ** 2 ) / ( 2 * b * b / 9 ) )
		
		sm <- 
			sm / sum( sm )
	
		for( i in 1 : nrow( g ) ) {
		
			cat( paste0( as.character( perc$date.month[ i ] ), "  " ) )
		
			for( j in c( -b : b ) ) {
			
				id <- 
					min( max( i + j, 1 ), nrow( g ) )
				
				for( k in 2 : ncol( perc ) ) {
				
					perc[ i, k ] <- perc[ i, k ] + sm[ j + 1 + b ] * g[ id, k ] } } }
		
		cat( "\n" ) } }

perc.melt <- 
	melt( perc, "date.month" ) 

perc.melt$value <-
	perc.melt$value / ( length( sub.iters ) * iters )

( plot3 <-
	ggplot( ) +
		geom_line( data = perc.melt, aes( date.month, value, group = variable, col = variable ), alpha = 1 ) +
		geom_point( data = d, aes( as.factor( date ), y ), col = "gray", alpha = .1 ) +
		theme_classic( ) )

ggsubplot( 
	plot2,
	plot3,
	layout = t( matrix( c( 1, 1, 2, 2 ), ncol = 2 ) ) )

ggsave( plot = plot3, filename = "plotperI10P5B12.png" )
