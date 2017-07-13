rm( list = ls( ) )

# install.packages( "devtools" )
# 
# devtools::install_github( "TPeschel/hlpr4life" )

library( hlpr4life )

setwd( "~/LIFE/life-for-postgraduates/Practise/2/" )

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

rename.column <-
	function( d, old.column.name, new.column.name ) {
		names( d )[ names( d ) == old.column.name ] <- 
			new.column.name 
		d }

rename.columns <-
	function( d, old.column.names, new.column.names ) {
		
		for( i in c( 1 : length( old.column.names ) ) ) {
			
			d <- 
				rename.column( d, old.column.names[ i ], new.column.names[ i ] ) }
		d }

# create.data.frame.for.percentile.curve.calculation.via.columns <-
# 	function( value, age, percentiles = c( 0 : 100 ) ) {
# 
# 		rows <-
# 			length( value )
# 
# 		rows.age <-
# 			length( age )
# 
# 		if( rows != rows.age ) {
# 
# 			print( "rows.value and row.age have to have the same length" )
# 			return( )
# 		}
# 
# 		data.frame(
# 			value  = value,
# 			age    = age,
# 			eval(
# 				parse(
# 					text =
# 						c(
# 							"data.frame(",
# 							c(
# 								paste0(
# 									"p",
# 									percentiles[ 1 : ( length( percentiles ) - 1 ) ],
# 									"=rep(0,rows)," ),
# 								paste0(
# 									"p",
# 									percentiles[ length( percentiles) ],
# 									"=rep(0,rows))" ) ) ) ) ),
# 			mu     = rep( 0, rows ),
# 			sd     = rep( 0, rows ),
# 			sigma  = rep( 0, rows ),
# 			low    = rep( 0, rows ),
# 			high   = rep( 0, rows ),
# 			lambda = rep( 0, rows ),
# 			nu     = rep( 0, rows ) ) }
# 
# create.data.frame.for.percentile.curve.calculation.via.data.frame <-
# 	function( data.frame, value.name, age.name, percentiles = c( 0 : 100 ) ) {
# 
# 		return(
# 			create.data.frame.for.percentile.curve.calculation.via.columns(
# 				data.frame[ , names( data.frame ) == value.name ],
# 				data.frame[ , names( data.frame ) == age.name ],
# 				percentiles ) ) }

n <-
	50000

all.ages <-
	seq( from = 3.56, to = 19.92, length.out = n ) + rnorm( n, 0, .5 )

d <-
	data.frame(
		alter = sort( all.ages ),
		motivation = ( 3 + ( 2 * cos( .5 +.00025 * c( 1 : n ) ) ** 2 ) * rnorm( n, 3, 2 ) ) + ( 4 + cos( .001 * c( 1 : n ) ) ) + ( .0001 * c( 1 : n - n / 2 ) ) ** 3 )

d$motivation <-
	10 * log10( 1 + abs( d$motivation ) + d$motivation ) + d$motivation

d$age.bins <-
	cut(
		x = d$alter, 
		breaks = seq( floor( min( d$alter ) ), ceiling( max( d$alter ) ), by = 1 / 12 ),
		labels = round( seq( floor( min( d$alter ) ), ceiling( max( d$alter ) ) - 1 / 12, by = 1 / 12 ), 2 ) )

dense <-
	.1 + .9 * cos( 2 * 3.142 * c( 1 : n ) / n ) ** 2

summary( dense )

d$motivation[ runif( length( dense ), 0, 1 ) < dense ] <-
	NA

d <-
	na.omit( d )

( plot1 <-
		ggplot( d ) +
		geom_point( aes( alter, motivation ), alpha = .1 ) +
		theme_bw( ) )

# create.data.frame.for.percentile.curve.calculation.via.data.frame( 
# 	data.frame = d, 
# 	value.name = "alter",
# 	age.name = "age.bins", 
# 	percentiles = 10 * c( 0 : 10 ) )
# 
# create.data.frame.for.percentile.curve.calculation.via.columns( 
# 	d$motivation, 
# 	d$age.bins,
# 	percentiles = 10 * c( 0 : 10 ) )

length(unique( d$age.bins  ) )
length( levels( d$age.bins ) )

compute.percentiles <-
	function( data.frame.with.age.bins_value, bins = 11, percentiles = c( 0, 0.1, 1, 2.5, 5, 10 * c( 1 : 9 ), 95, 97.5, 99, 99.9, 100 ) ) {
		
		perc <-
			eval(
				parse(
					text =
						c(
							"data.frame.with.age.bins_value %>% group_by( age.bins ) %>% summarise(",
							paste0(
								"p",
								percentiles,
								"=quantile(value,.01 * ", percentiles, ")," ),
							"mu     = mean( value ),
							sigma  = sd( value ),
							var    = var( value ),
							low    = mu - sigma,
							high   = mu + sigma,
							mn     = min( value ),
							mx     = max( value ) )" ) ) )

		sm <-
			exp( -( c( -bins : bins ) ** 2 ) / ( 2 * bins * bins / 10 ) )
		
		sm <-
			sm / sum( sm )

		perc.boxed <-
			perc
		
		perc.boxed[ , 2 : ( 1 + length( percentiles ) ) ] <-
			0
		
		for( i in 1 : nrow( perc ) ) {

			cat( paste0( as.character( perc$age.bins[ i ] ), "  " ) )

			for( j in c( -bins : bins ) ) {

				id <-
					min( max( i + j, 1 ), nrow( perc ) )

				for( k in 2 : ( 1 + length( percentiles ) ) ) {

					perc.boxed[ i, k ] <-
						perc.boxed[ i, k ] + sm[ j + 1 + bins ] * perc[ id, k ] } } }
		
		cat( "\n" )

		perc.boxed }

d <- 
	rename.columns( d, old.column.names = c( "motivation", "alter" ), new.column.names = c( "value", "age" ) )

for( bins in 0 : 51 ) {
	
	print( 51 - bins ) 
	
	perc <-
		compute.percentiles( d, bins )
	
	perc.melt <-
		melt( perc, "age.bins" )
	
	(
		ggplot( ) +
		geom_point( data = d, aes( age.bins, value ), col = "black",  alpha = .1 ) +
		scale_x_discrete( "age" ) +
		#geom_point( data = d, aes( age.bins, mn ), col = "green" ) +
		# geom_point( data = d, aes( age.bins, mx ), col = "red" ) +
		# geom_point( data = d, aes( age.bins, mu ), col = "blue" ) +
		# geom_point( data = d, aes( age.bins, low ), col = "yellow" ) +
		# geom_point( data = d, aes( age.bins, high ), col = "yellow" ) +
		# scale_color_discrete( guide = F ) +
		geom_point( data = perc.melt[ perc.melt$variable %in% c( "mn", "max", "mu", "low", "high" ), ], aes( age.bins, value, group = variable, col = variable ), alpha = 1 ) +
		geom_line( data = perc.melt[ !perc.melt$variable %in% c( "var", "sigma" ), ], aes( age.bins, value, group = variable, col = variable ), alpha = 1 ) +
		theme_base( ) +
		annotate( geom = "text", x = length( levels( perc.melt$age.bins ) ) / 2.4, y = 5, label = paste0( "bins: ", ( 2 * bins + 1 ) ), size = 10 ) +
 		theme( axis.text.x = element_text( angle = 90, size = 7 ) ) )

	bins.str <-
		as.character( bins )
	
	bins.str <-
		paste0( paste0( rep( x = "0", length.out = 4 - nchar( bins.str ) ), collapse = "" ), bins.str )
	
	ggsave( filename = paste0( "plot", nrow( d ), "B", bins.str, ".png" ), width = 16, height = 8 ) }
# 
# 
# 
# for( bins in c( 1 : 23 ) ) {
# 	
# 	# bins <-
# 	# 	5
# 	
# 	sample.prob <-
# 		1
# 	
# 	if( sample.prob < 1 ) {
# 		
# 		df <-
# 			d[ sample( c( T, F ), nrow( d ), T, prob = c( sample.prob, ( 1 - sample.prob ) ) ), ] 
# 	} else {
# 		
# 		df <-
# 			d
# 	}
# 	
# 	perc <-
# 		quntls(
# 			df = df,
# 			date.bins = "date.bins" )
# 	
# 	perc[ , -1 ] <-
# 		0
# 	
# 	for( it in c( 1 : iters ) ) {
# 		
# 		print( paste( "iteration: ", it ) )
# 		
# 		smpl <-
# 			d[ sample( c( T, F ), nrow( d ), T, prob = c( sample.prob, ( 1 - sample.prob ) ) ), ]
# 		
# 		g <-
# 			quntls( smpl, "date.bins" )
# 		
# 		sm <- 
# 			#rep( 1, 2 * b + 1 ) / ( 2 * b + 1 ) #c( -b : b )
# 			exp( -( c( -bins : bins ) ** 2 ) / ( 2 * bins * bins / 10 ) )
# 		
# 		sm <- 
# 			sm / sum( sm )
# 		
# 		for( i in 1 : nrow( g ) ) {
# 			
# 			cat( paste0( as.character( perc$date.bins[ i ] ), "  " ) )
# 			
# 			for( j in c( -bins : bins ) ) {
# 				
# 				id <- 
# 					min( max( i + j, 1 ), nrow( g ) )
# 				
# 				for( k in 2 : ncol( perc ) ) {
# 					
# 					perc[ i, k ] <- 
# 						perc[ i, k ] + sm[ j + 1 + bins ] * g[ id, k ] } } }
# 		
# 		cat( "\n" ) }
# 	
# 	d %<>% 
# 		group_by( date.bins ) %>%
# 		mutate( 
# 			mn = min( y ),
# 			mx = max( y ), 
# 			mu = mean( y ), 
# 			sigma = sd( y ), 
# 			low = mu - sigma, 
# 			high = mu + sigma )
# 	
# 	perc.melt <- 
# 		melt( perc, "date.bins" ) 
# 	
# 	perc.melt$value <-
# 		perc.melt$value / iters
# 	
# 	( plot3 <-
# 			ggplot( ) +
# 			geom_point( data = d, aes( date.bins, y ), col = "black",  alpha = .3 ) +
# 			#scale_x_discrete( d$date.bins[ seq( 1, brks, length.out = 10 ) ] ) +
# 			geom_point( data = d, aes( date.bins, mn ), col = "green" ) + 
# 			geom_point( data = d, aes( date.bins, mx ), col = "red" ) + 
# 			geom_point( data = d, aes( date.bins, mu ), col = "blue" ) + 
# 			geom_point( data = d, aes( date.bins, low ), col = "yellow" ) + 
# 			geom_point( data = d, aes( date.bins, high ), col = "yellow" ) + 
# 			scale_color_discrete( guide = F ) +
# 			geom_line( data = perc.melt, aes( date.bins, value, group = variable, col = variable ), alpha = 1 ) +
# 			theme_base( ) +
# 			annotate( geom = "text", x = brks / 2, y = 12, label = paste0( "bins: ", ( 2 * bins + 1 ) ), size = 10 ) +
# 			theme( axis.text.x = element_text( angle = 90, size = 7 ) ) ) 
# 	
# 	# ggsubplot( 
# 	# 	plot2,
# 	# 	plot3,
# 	# 	layout = t( matrix( c( 1, 1, 2, 2 ), ncol = 2 ) ) )
# 	
# #	ggsave( plot = plot3, filename = paste0( "plOtN", n, "I", iters, "P", sample.prob, "B", bins, ".png" ), width = 16, height = 8 )
# }
# 
# 
# 
#  