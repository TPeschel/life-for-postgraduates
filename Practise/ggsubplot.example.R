rm( list = ls( ) )
library( ggthemes )
library( life.helper )
library( ggplot2 )
library( grid )

d1 <-
	data.frame(
		x = rnorm( 100, 100, 100 ),
		y = rbinom( 100, 50, prob = .5 ) )

d2 <-
	data.frame(
		x = rnorm( 100, 100, 100 ),
		y = runif( 100, 0, 100 ) )

d3 <-
	data.frame(
		freq = rf( 200, 100, 100 ),
		grps = as.factor( c( "male", "female" )[ match( floor( runif( 200, 0, 2 ) ), c( 0, 1 ) ) ] ) )

p1 <-
	ggplot( d1 ) +
	geom_point( aes( x, y ) ) +
	theme_bw( )

p2 <-
	ggplot( d2 ) +
	geom_point( aes( x, y ) ) +
	theme_pander( )

p3 <-
	ggplot( d3 ) +
	geom_histogram( aes( freq, fill = grps ) ) +
	theme_excel( ) +
	scale_fill_manual( "sex", values = c( "#FF4000", "#0040FF" ), guide = F ) +
	facet_grid( . ~ grps )

layout <-
	matrix( c( 1, 1, 4, 2, 3, 4 ), ncol = 2, nrow = 3 )

ggsubplot( p1, p2, p3,ggplot( polyfy( x = x<-.01*c( -100 : 100), y=exp( -5 * x * x ),g=c(rep("A",70), rep("B",61),rep("C",70))))+geom_polygon( aes(X,Y,fill=G)), layout = layout )

ggplot( polyfy( x = x<-.01*c( -100 : 100), y=exp( -5 * x * x ),g=c(rep("A",70), rep("B",61),rep("C",70))))+geom_polygon( aes(X,Y,fill=G))
