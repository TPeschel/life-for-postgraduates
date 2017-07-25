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

load( "../ThomasBerger/r/data_sprech.Rda" )

d <-
	data.sprech[ , c( "age", "sex",  "F0_SPRECH_2" ) ]

brks <-
	as.numeric( round( max( d$age ) - min( d$age ) ) * 12 )

n <-
	nrow( d )

# d <-
# 	data.frame(
# 		date = seq( from = from, to = to, length.out = n ),
# 		y = ( 1 + 2 * cos( .001 * c( 1 : n ) ) ** 2 ) * rnorm( n ) * cos( .0001 * c( 1 : n ) ) + ( .00015 * c( 1 : n - n / 2 ) ) ** 2 )

d$age.bins <-
	cut(
		d$age,
		breaks = brks )

d <-
	na.omit( d )

( plot1 <-
		ggplot( d[ d$sex == "male", ] ) +
		geom_point( aes( age, F0_SPRECH_2 ), alpha = .1 ) +
		theme_bw( ) )

arrange( d, age )

summary( d$age )

quntls <-
	function( df, y = "y", age.bins = "age.bins", probs = c( 0, .001, .01, .025, .05, .1, .25, .5, .75, .9, .95, .975, .99, .999, 1 ) ) {
		
		txt <-
			paste0(
				'ss <- df %>% group_by(', age.bins, ') %>% summarise(',
				paste( paste0( "p", 100 * probs[ 1 ] ), "=", paste0( "quantile(", y, ",", probs[ 1 ], ")" ) ) )
		
		for( p in probs ) {
			
			txt <-
				paste0( txt, ",", paste( paste0( "p", 100 * p ),"=", paste0( "quantile(", y, ",", p, ")" ) ) ) }
		
		txt <-
			paste0( txt, ")" )
		
		eval( parse( text = txt ) )
		
		ss }

names( d ) <-
	c( "age", "sex", "y", "age.bins" )

d.males <-
	d[ d$sex == "male", ]

d.females <-
	d[ d$sex == "female", ]

quntls( d.males, "y", "age.bins" )

( plot2 <-
		ggplot( ) +
		geom_line( data = melt( quntls( d.males, "y", "age.bins" ), c( "age.bins" ) ), aes( age.bins, value, group = variable, col = variable ), alpha = 1 ) +
		geom_point( data = d.males, aes( age.bins, y ), col = "gray", alpha = .5 ) +
		theme_classic( ) )

iters <-
	1

# x.scale <- 
# 	sample( d$age.bins, 10 )

d.loop <-
	d.males

for( bins in c( 1 : 23 ) ) {

	# bins <-
	# 	5

	sample.prob <-
		1
	
	if( sample.prob < 1 ) {
		
		d.l <-
			d.loop[ sample( c( T, F ), nrow( d.loop ), T, prob = c( sample.prob, ( 1 - sample.prob ) ) ), ] 
	} else {
	
		d.l <- 
			d.loop	
	}
	
	perc <-
		quntls(
			df = d.l,
			y = "y",
			age.bins = "age.bins" )
	
	perc[ , -1 ] <-
		0
	
	for( it in c( 1 : iters ) ) {
	
		print( paste( "iteration: ", it ) )
		
		d.l <-
			d.loop[ sample( c( T, F ), nrow( d.loop ), T, prob = c( sample.prob, ( 1 - sample.prob ) ) ), ]
		
		g <-
			quntls( d.l, "y", "age.bins" )
		
		sm <- 
			#rep( 1, 2 * b + 1 ) / ( 2 * b + 1 ) #c( -b : b )
			exp( -( c( -bins : bins ) ** 2 ) / ( 2 * bins * bins / 10 ) )
			
		sm <- 
		 	sm / sum( sm )
		
		for( i in 1 : nrow( g ) ) {
			
			cat( paste0( as.character( perc$age.bins[ i ] ), "  " ) )
			
			for( j in c( -bins : bins ) ) {
				
				id <- j + i
				
				if( id < 1 ) id <- 1 - id
				
				if( nrow( g ) < id ) id <- 2 * nrow( g ) - id

				for( k in 2 : ncol( perc ) ) {
					
					perc[ i, k ] <- 
						perc[ i, k ] + sm[ j + 1 + bins ] * g[ id, k ] } } }
		
		cat( "\n" ) }
	
	d.l %<>% 
		group_by( age.bins ) %>%
		mutate( 
			mn = min( y ),
			mx = max( y ), 
			mu = mean( y ), 
			sigma = sd( y ), 
			low = mu - sigma, 
			high = mu + sigma )
	
	perc.melt <- 
		melt( perc, "age.bins" ) 
	
	perc.melt$value <-
		perc.melt$value / iters
	
	( plot3 <-
			ggplot( ) +
			geom_point( data = d.l, aes( age.bins, y ), col = "black",  alpha = .3 ) +
			#scale_x_discrete( d$age.bins[ seq( 1, brks, length.out = 10 ) ] ) +
			geom_point( data = d.l, aes( age.bins, mn ), col = "green" ) + 
			geom_point( data = d.l, aes( age.bins, mx ), col = "red" ) + 
			geom_point( data = d.l, aes( age.bins, mu ), col = "blue" ) + 
			geom_point( data = d.l, aes( age.bins, low ), col = "yellow" ) + 
			geom_point( data = d.l, aes( age.bins, high ), col = "yellow" ) + 
			scale_color_discrete( guide = F ) +
			geom_line( data = perc.melt, aes( age.bins, value, group = variable, col = variable ), alpha = 1 ) +
			theme_base( ) +
			annotate( geom = "text", x = brks / 2, y = max( d.l$y ), label = paste0( "bins: ", ( 2 * bins + 1 ) ), size = 10 ) +
			theme( axis.text.x = element_text( angle = 90, size = 7 ) ) ) 
	
	# ggsubplot( 
	# 	plot2,
	# 	plot3,
	# 	layout = t( matrix( c( 1, 1, 2, 2 ), ncol = 2 ) ) )
	
	ggsave( plot = plot3, filename = paste0( "ssplotStimmeSprech2NFreeEnds", n, "I", iters, "P", sample.prob, "B", bins, ".png" ), width = 12, height = 8 )
}
		
