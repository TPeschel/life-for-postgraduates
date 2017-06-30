rm( list = ls( ) )

library( ggthemes )
library( life.helper )
library( ggplot2 )
library( grid )
library ( reshape2 )

colors.sex <-
	c( "#0000FF", "#FF0000" )

colors.sizes <-
	c( "#000000", "#008000", "#00FF00" )

colors.sex.sizes <-
	c( "#FF0000", "#0000FF", 
	   "#FF8000", "#0080FF",
	   "#FFFF00", "#00FFFF" )

colors.sizes.sex <-
	c( "#FF8000","#FF0000",  "#FFFF00",
	   "#0080FF","#0000FF",  "#00FFFF" )
mu <-
	0

sig <-
	1

d0 <-
	data.frame(
		event = x <- .05 * c( -100 : 100 ),
		density.at.event = y <- exp( - ( x - mu ) * ( x - mu ) / ( 2 * sig * sig ) ) / ( sig * sqrt( 2 * pi ) ),
		density.at.event.times.event = xy <- x * y,
		probability.for.beeing.before.event = cumsum( y ) / sum( y ),
		expectation.for.event = cumsum( xy ) / sum( y ) )
n <-
	3000

d1 <-
	data.frame(
		x = rnorm( n, 1.78, .1 ),
		y = rbinom( n, 200, prob = c( .63 * .63, .63, 1 ) / ( .63 + .63 * .63 + 1 ) ),
		sex = as.factor( sample( c( "male", "female" ), n, T, c( .4, .6 ) ) ),
		sizes = as.factor( 
			sample( 
				c(
					"small",
					"normal",
					"tall" ), 
				n, 
				T, 
				prob = c( .3, .5, .2 ) ) ) )

d1$sizes <-
	relevel( d1$sizes, "small" )

d1$sex <-
	relevel( d1$sex, "male" )

ggsubplot(
	ggplot( 
		melt( 
			d0, 
			c( "event" ) ) ) +
	geom_path( aes( event, value, col = variable ), alpha = .25, size = 3 ) +
	geom_path( aes( event, value, col = variable ), alpha = 1, size = .2 ) +
	scale_x_continuous( breaks = c( -5 : 5 ) ) +
	scale_y_continuous( breaks = seq( -.4, 1.1, by = .2 ) ) +
	theme_bw( ),
	ggplot( 
		polyfy( 
			x = x<-.01*c( -100 : 100), 
			y = exp( -5 * x * x ),
			g = c(
				rep( "A", 70 ),
				rep( "B", 61),
				rep( "C", 70 ) ) ) ) +
		geom_polygon( 
			aes(
				X,
				Y,
				fill = G ) ) +
		theme_calc( ),
	cols = 1 )

ggsubplot(
	ggplot( d1 ) +
		geom_point( aes( x, y, col = sex ), alpha = .125 ) +
		geom_rug( aes( x, y, col = sex ), alpha = .125 ) +
		scale_color_manual( values = colors.sex ) +
		theme_bw( ),
	ggplot( d1 ) +
		geom_point( aes( x, y, col = sizes ), alpha = .125 ) +
		geom_rug( aes( x, y, col = sizes ), alpha = .125 ) +
		scale_color_manual( values = colors.sizes ) +
		theme_bw( ),
	ggplot( d1 ) +
		geom_point( aes( x, y, col = paste0( sex, " ", sizes ) ), alpha = .2 ) +
		geom_rug( aes( x, y, col = paste0( sex, " ", sizes ) ), alpha = .1 ) +
		scale_color_manual( "kind of human", values = colors.sizes.sex ) +
		theme_bw( ),
	ggplot( d1 ) +
		geom_point( aes( x, y, col = paste0( sex, "-", sizes ) ), alpha = .125 ) +
		scale_color_manual( values = colors.sizes.sex, guide = F ) +
		facet_grid( sizes ~ sex ) +
		theme_pander( ),
	ggplot( d1 ) +
		geom_point( aes( x, y, col = paste0( sex, "-", sizes ) ), alpha = .125 ) +
		scale_color_manual( values = colors.sizes.sex, guide = F ) +
		facet_grid( sex ~ sizes ) +
		theme_pander( ),
	ggplot( d1 ) +
		geom_point( aes( x, y, col = paste0( sex, "-", sizes ) ), alpha = .125 ) +
		scale_color_manual( values = colors.sizes.sex, guide = F ) +
		facet_grid( sizes ~ sex ) +
		coord_flip( ) +
		theme_pander( ),
	ggplot( d1 ) +
		geom_point( aes( x, y, col = paste0( sex, "-", sizes ) ), alpha = .125 ) +
		scale_color_manual( values = colors.sizes.sex, guide = F ) +
		facet_grid( sex ~ sizes ) +
		coord_flip( ) +
		theme_pander( ),
	ggplot( d1 ) +
		geom_histogram( aes( sizes, fill = sizes ), stat = "count" ) +
		theme_solarized( ) +
		scale_fill_manual( values = colors.sizes, guide = F ) +
		facet_grid( . ~ sex ),
	ggplot( d1 ) +
		geom_histogram( aes( sex, fill = sizes ), stat = "count" ) +
		theme_solarized( ) +
		scale_fill_manual( values = colors.sizes, guide = F ),
	ggplot( d1 ) +
		geom_histogram( aes( sizes, fill = sex ), stat = "count" ) +
		theme_solarized( ) +
		scale_fill_manual( "sex", values = colors.sex, guide = F ),
	ggplot( d1 ) +
		geom_histogram( aes( sex, fill = sex ), stat = "count" ) +
		theme_solarized( ) +
		scale_fill_manual( values = colors.sex, guide = F ) +
		facet_grid( . ~ sizes ),
	ggplot( d1 ) +
		geom_point( aes( x, y, col = paste0( sizes, " ", sex ) ), alpha = .1 ) +
		geom_rug( aes( x, y, col = paste0( sizes, " ", sex ) ), alpha = .1) +
		scale_color_manual( "kind of human", values = colors.sizes.sex ) +
		theme_light( ) +
		coord_flip( ),
	ggplot( d1 ) +
		geom_boxplot( inherit.aes = T, aes( x, y, col = paste0( sizes, " ", sex ), fill = paste0( sizes, " ", sex ) ), alpha = .75 ) +
		geom_rug( aes( x, y, col = paste0( sizes, " ", sex ), fill = paste0( sizes, " ", sex ) ), alpha = .1 ) +
		scale_color_manual( "kind of human", values = colors.sex.sizes ) +
		scale_fill_manual( "kind of human", values = colors.sex.sizes ) +
		theme_light( ) +
		facet_grid( sex ~ sizes ),
	ggplot( d1 ) +
		geom_boxplot( inherit.aes = T, aes( y, x, col = paste0( sizes, " ", sex ), fill = paste0( sizes, " ", sex ) ), alpha = .75 ) +
		geom_rug( aes( y, x, col = paste0( sizes, " ", sex ), fill = paste0( sizes, " ", sex ) ), alpha = .1 ) +
		coord_flip( ) +
		scale_color_manual( "kind of human", values = colors.sex.sizes ) +
		scale_fill_manual( "kind of human", values = colors.sex.sizes ) +
		theme_light( ) +
		facet_grid( sex ~ sizes ),
	layout =
		t(
			matrix(
				c(
					4,  1,  2,  5,
					6,  3,  3,  7,
					6,  3,  3,  7,
					8, 12, 12, 11,
					9, 14, 13, 10
				),
				nrow = 4 ) ) )
