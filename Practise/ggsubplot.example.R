rm( list = ls( ) )

library( ggthemes )
library( life.helper )
library( ggplot2 )
library( grid )
library ( reshape2 )

colors.sex <-
	c( "#0000FF", "#FF0000" )

colors.sizes <-
	c( "#404040", "#707070", "#C0C0C0" )

colors.sex.sizes <-
	c( "#802020", "#401010", "#FF4040",
	   "#202080", "#101040", "#4040FF" )
mu <-
	0

sig <-
	1

d0 <-
	data.frame(
		x = x <- .05 * c( -100 : 100 ),
		density = y <- exp( - ( x - mu ) * ( x - mu ) / ( 2 * sig * sig ) ) / ( sig * sqrt( 2 * pi ) ),
		density.times.x = xy <- x * y,
		probability = cumsum( y ) / sum( y ),
		expectation.value = cumsum( xy ) / sum( y ) )

d1 <-
	data.frame(
		x = rnorm( 1000, 1.78, .1 ),
		y = rbinom( 1000, 1000, prob = c( .68 * .68, .68, 1 ) / ( .68 + .68 * .68 + 1 ) ),
		sex = as.factor( sample( c( "male", "female" ), 100, T, c( .4, .6 ) ) ),
		sizes = as.factor( 
			sample( 
				c(
					"small",
					"normal",
					"tall" ), 
				100, 
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
			c( "x" ) ) ) +
	geom_path( aes( x, value, col = variable ), alpha = .25, size = 3 ) +
	geom_path( aes( x, value, col = variable ), alpha = 1, size = .2 ) +
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
		geom_point( aes( x, y, col = sex ), alpha = .25 ) +
		geom_rug( aes( x, y, col = sex ), alpha = .25 ) +
		scale_color_manual( values = colors.sex ) +
		theme_bw( ),
	ggplot( d1 ) +
		geom_point( aes( x, y, col = sizes ), alpha = .25 ) +
		geom_rug( aes( x, y, col = sizes ), alpha = .25 ) +
		scale_color_manual( values = colors.sizes ) +
		theme_bw( ),
	ggplot( d1 ) +
		geom_point( aes( x, y, col = paste0( sizes, " ", sex ) ), alpha = 1 ) +
		geom_rug( aes( x, y, col = paste0( sizes, " ", sex ) ), alpha = .5 ) +
		scale_color_manual( "kind of human", values = colors.sex.sizes ) +
		theme_bw( ),
	ggplot( d1 ) +
		geom_point( aes( x, y, col = paste0( sex, "-", sizes ) ), alpha = .25 ) +
		scale_color_manual( values = colors.sex.sizes, guide = F ) +
		facet_grid( sizes ~ sex ) +
		theme_pander( ),
	ggplot( d1 ) +
		geom_point( aes( x, y, col = paste0( sex, "-", sizes ) ), alpha = .25 ) +
		scale_color_manual( values = colors.sex.sizes, guide = F ) +
		facet_grid( sex ~ sizes ) +
		theme_pander( ),
	ggplot( d1 ) +
		geom_point( aes( x, y, col = paste0( sex, "-", sizes ) ), alpha = .25 ) +
		scale_color_manual( values = colors.sex.sizes, guide = F ) +
		facet_grid( sizes ~ sex ) +
		coord_flip( ) +
		theme_pander( ),
	ggplot( d1 ) +
		geom_point( aes( x, y, col = paste0( sex, "-", sizes ) ), alpha = .25 ) +
		scale_color_manual( values = colors.sex.sizes, guide = F ) +
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
		scale_color_manual( "kind of human", values = colors.sex.sizes ) +
		theme_light( ) +
		coord_flip( ),
	ggplot( d1 ) +
		geom_boxplot( aes( x, y ), alpha = .1 ) +
		geom_rug( aes( x, y ), alpha = .1) +
		#scale_color_manual( "kind of human", values = colors.sex.sizes ) +
		theme_light( ) +
		facet_grid( sex ~ sizes ),
	layout =
		t(
			matrix(
				c(
					4,  1,  2,  5,
					6,  3,  3,  7,
					6,  3,  3,  7,
					8,  3,  3, 11,
					9, 12, 13, 10
				),
				nrow = 4 ) ) )

p4 <-
	ggplot( d1 ) +
	geom_point( aes( x, y, col = sex ), alpha = .25 ) +
	geom_rug( aes( x, y, col = sex ) ) +
	scale_color_manual( values = colors.sex ) +
	theme_bw( )

p5 <-
	ggplot( d1 ) +
	geom_point( aes( x, y, col = paste0( sex, "-", sizes ) ), alpha = .25 ) +
	scale_color_manual( values = colors.sex.sizes, guide = F ) +
	facet_grid( sizes ~ sex ) +
	theme_pander( )

# p6 <-
# 	ggplot( d1 ) +
# 	geom_point( aes( x, y, col = paste0( sizes, "-", sex ) ), alpha = .25 ) +
# 	scale_color_manual( values = colors.sex.sizes, guide = F ) +
# 	coord_flip( ) +
# 	facet_grid( sizes ~ sex ) +
# 	theme_pander( )
# 
p6 <-
	ggplot( d1 ) +
	geom_point( aes( x, y, col = paste0( sizes, " ", sex ) ), alpha = .25 ) +
	geom_rug( aes( x, y, col = paste0( sizes, " ", sex ) ) ) +
	scale_color_manual( "kind of human", values = colors.sex.sizes ) +
	theme_bw( )
# 
# p8 <-
# 	ggplot( d1 ) +
# 	geom_point( aes( x, y, col = paste0( sex, "-", sizes ) ), alpha = .5 ) +
# 	geom_rug( aes( x, y, col = paste0( sex, "-", sizes ) ) ) +
# 	scale_color_manual( "sex-size", values = colors.sex.sizes ) +
# 	theme_bw( )

p7 <-
	ggplot( d1 ) +
	geom_histogram( aes( sex, fill = sex ), stat = "count" ) +
	theme_solarized( ) +
	scale_fill_manual( "sex", values = colors.sex, guide = F ) +
	facet_grid( . ~ sizes )

p8 <-
	ggplot( d1 ) +
	geom_histogram( aes( sizes, fill = sizes ), stat = "count" ) +
	theme_solarized( ) +
	scale_fill_manual( "sizes", values = colors.sizes, guide = F ) +
	facet_grid( . ~ sex )

p9 <-
	ggplot( d1 ) +
	geom_histogram( aes( sex, fill = sex ), stat = "count" ) +
	theme_solarized( ) +
	scale_fill_manual( "sex", values = colors.sex, guide = F )

p10 <-
	ggplot( d1 ) +
	geom_histogram( aes( sizes, fill = sizes ), stat = "count" ) +
	theme_solarized( ) +
	scale_fill_manual( "sizes", values = colors.sizes, guide = F )

layout <-
	t( 
		matrix( 
			c( 3, 1, 2, 4,
			   5, 6, 7, 14,
			   13, 10, 11, 12,
			   9, 8, 15, 16 ), 
			ncol = 4, 
			nrow = 4 ) )

ggsubplot( 
	p1, 
	p2, 
	p3, 
	p4,
	p5,
	p6,
	p7,
	p8,
	p9,
	p10,
	p5 + coord_flip( ),
	p5 + coord_flip( ),
	p5 + facet_grid( sizes ~ sex ),
	p5 + facet_grid( sex ~ sizes ),
	p5 + facet_grid( sizes ~ sex ) + coord_flip( ),
	p5 + facet_grid( sex ~ sizes ) + coord_flip( ),
	layout = layout )
