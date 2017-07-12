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

brks <-
	as.numeric( round( difftime( to, from ) / ( 365.25 / 12 ) ) )

n <-
	5000

d <-
	data.frame(
		date = seq( from = from, to = to, length.out = n ),
		y = ( 1 + 2 * cos( .001 * c( 1 : n ) ) ** 2 ) * rnorm( n ) * cos( .0001 * c( 1 : n ) ) + ( .0005 * c( 1 : n - n / 2 ) ) ** 2 )


( plot1 <-
		ggplot( d ) +
		geom_point( aes( date, y ), alpha = .1 ) +
		theme_bw( ) )

arrange( d, date )

summary( d$date )

d$date.bins <-
	cut(
		d$date,
		breaks = brks )

quntls <-
	function( df, date.bins = "date.bins", probs = c( 0, .001, .01, .025, .05, .1, .25, .5, .75, .9, .95, .975, .99, .999, 1 ) ) {
		
		txt <-
			paste0(
				'ss <- df %>% group_by(', date.bins, ') %>% summarise(',
				paste( paste0( "p", 100 * probs[ 1 ] ), "=", paste0( "quantile(y,", probs[ 1 ], ")" ) ) )
		
		for( p in probs ) {
			
			txt <-
				paste0( txt, ",", paste( paste0( "p", 100 * p ),"=", paste0( "quantile(y,", p, ")" ) ) ) }
		
		txt <-
			paste0( txt, ")" )
		
		eval( parse( text = txt ) )
		
		ss }

# quntls( d, "date.bins" )
# 
# ( plot2 <-
# 		ggplot( ) +
# 		geom_line( data = melt( quntls( d, "date.bins" ), "date.bins" ), aes( date.bins, value, group = variable, col = variable ), alpha = 1 ) +
# 		geom_point( data = d, aes( as.factor( date ), y ), col = "gray", alpha = .1 ) +
# 		theme_classic( ) )

iters <-
	1

bins <-
	23

sample.prob <-
	1

perc <-
	quntls(
		df = d[ sample( c( T, F ), nrow( d ), T, prob = c( sample.prob, ( 1 - sample.prob ) ) ), ],
		date.bins = "date.bins" )

perc[ , -1 ] <-
	0

for( it in c( 1 : iters ) ) {
	
	print( paste( "iteration: ", it ) )
	
	smpl <-
		d[ sample( c( T, F ), nrow( d ), T, prob = c( sample.prob, ( 1 - sample.prob ) ) ), ]
	
	g <-
		quntls( smpl, "date.bins" )
	
	sm <- 
		#rep( 1, 2 * b + 1 ) / ( 2 * b + 1 ) #c( -b : b )
		exp( -( c( -bins : bins ) ** 2 ) / ( 2 * bins * bins / 10 ) )
		
	sm <- 
	 	sm / sum( sm )
	
	for( i in 1 : nrow( g ) ) {
		
		cat( paste0( as.character( perc$date.bins[ i ] ), "  " ) )
		
		for( j in c( -bins : bins ) ) {
			
			id <- 
				min( max( i + j, 1 ), nrow( g ) )
			
			for( k in 2 : ncol( perc ) ) {
				
				perc[ i, k ] <- 
					perc[ i, k ] + sm[ j + 1 + bins ] * g[ id, k ] } } }
	
	cat( "\n" ) }

d %<>% 
	group_by( date.bins ) %>%
	mutate( 
		mn = min( y ),
		mx = max( y ), 
		mu = mean( y ), 
		sigma = sd( y ), 
		low = mu - sigma, 
		high = mu + sigma )

perc.melt <- 
	melt( perc, "date.bins" ) 

perc.melt$value <-
	perc.melt$value / iters

( plot3 <-
		ggplot( ) +
		geom_point( data = d, aes( as.factor( date ), y ), col = "black",  alpha = .3 ) +
		geom_point( data = d, aes( date.bins, mn ), col = "green" ) + 
		geom_point( data = d, aes( date.bins, mx ), col = "red" ) + 
		scale_color_discrete( guide = F ) +
		geom_line( data = perc.melt, aes( date.bins, value, group = variable, col = variable ), alpha = 1 ) +
		theme_classic( ) )

# ggsubplot( 
# 	plot2,
# 	plot3,
# 	layout = t( matrix( c( 1, 1, 2, 2 ), ncol = 2 ) ) )

ggsave( plot = plot3, filename = paste0( "plotI", iters, "P", sample.prob, ".png" ) )
	