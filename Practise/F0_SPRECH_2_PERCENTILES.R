#! /usr/bin/env Rscript
rm( list = ls( ) )

library( "hlpr4life" )

load.pkgs(
	c(
		"dplyr",
		"ggplot2",
		"ggthemes",
		"reshape2",
		"stringr",
		"lubridate" ) )

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
		
		perc.boxed[ , 2 : ncol( perc ) ] <-
			0
		
		for( i in 1 : nrow( perc ) ) {
			
			cat( paste0( as.character( perc$age.bins[ i ] ), "  " ) )
			
			for( j in c( -bins : bins ) ) {
				
				id <-
					min( max( i + j, 1 ), nrow( perc ) )
				
				#				for( k in 2 : ( 1 + length( percentiles ) ) ) {
				for( k in 2 : ncol( perc ) ) {
					
					perc.boxed[ i, k ] <-
						perc.boxed[ i, k ] + sm[ j + 1 + bins ] * perc[ id, k ] } } }
		
		cat( "\n" )
		
		perc.boxed }


setwd( "~/Schreibtisch/ThomasBerger/data/" )

load( "data_sprech.Rda" )	

setwd( "../gfx/" )

tm <-
	gsub( "[ \\-:]", "", Sys.time( ), perl = T )

dir.create( tm )

setwd( tm )

d <-
	data.sprech

names( d )

ggplot( d ) +
	geom_point( aes( age, F0_SPRECH_2, col = sex ) ) +
	facet_grid( sex ~ . ) +
	theme_bw( ) +
	scale_color_manual( values = c( "deeppink", "deepskyblue" ) ) 

d$age.bins <-
	cut(
		d$age,
		seq( floor( min( d$age ) ), ceiling( max( d$age ) ) + 1 / 12 , by = 1 / 12 ),
		round( seq( floor( min( d$age ) ), ceiling( max( d$age ) ), by = 1 / 12 ), 2 ) )

d$value <-
	d$F0_SPRECH_2

d <-
	d[ !is.na( d$value ), ]

bins.min <-
	1

bins.max <-
	51

for( bins in c( bins.min : bins.max ) ) {

	print( paste0( "compute.percentiles for females with 2 · ", bins, " + 1 months width" ) )
	
	p.m <-
		compute.percentiles( d[ d$sex == "male", ], bins = bins )
	
	p.m$sex <-
		"male"
	
	print( paste0( "compute.percentiles for males with 2 · ", bins, " + 1 months width" ) )
	
	p.f <-
		compute.percentiles( d[ d$sex == "female", ], bins = bins )
	
	p.f$sex <-
		"female"
	
	p <-
		rbind.data.frame( p.m, p.f )
	
	p <-
		p[ , !names( p ) %in% c( "var", "sigma" ) ]
	
	p.mlt <-
		melt( p, c( "age.bins", "sex" ) )
	
	print(
		ggplot( ) +
			geom_point( data = d, aes( age.bins, F0_SPRECH_2 ), alpha = .5 ) +
			geom_line(  data = p.mlt, aes( age.bins, value, group = variable, col = variable ) ) +
			facet_grid( sex ~ . ) +
			theme_bw( ) +
			theme( axis.text.x = element_text( angle = 90 , size = 6 ) ) +
			scale_color_discrete( ) )
	
	fname <-
		paste0( "plot_", str_pad( bins, ceiling( log10( bins.max ) ), pad = "0" ), ".png" )
	
	print( paste0( "save plot ", fname ) )
		   
	ggsave( file = fname, width = 16, height = 9 ) }
