rm( list = ls( ) )
n <- 1000
x <- rnorm( n, 10, 2 )
y <- 10 * x + rnorm( n, -100, 10 )

library( ggplot2 )
library( ggthemes )
library( hlpr4life )

b <- seq( floor( min( x ) ), ceiling( max( x ) ), by = .5 )
l <- b[ -1 ] - .25
length(b)
length(l)

ggplot( ) + geom_histogram( aes( x = cut( x = x, breaks = b, labels = l ), y ), stat = "identity", position = "identity" )
lm.y.x <-
	lm( y ~ x )
ggplot( ) + 
	geom_point( aes( lm.y.x$fitted.values, lm.y.x$effects ) ) + theme_few( )

table( lm.y.x$effects)

ggsubplot( 
	ggplot( ) + geom_point( aes( x, y ) )  + ggtitle( "scatter of y( x )" ) + theme_economist_white( ),
	ggplot( ) + geom_histogram( aes( x ) ) + ggtitle( "histo of x" ) + theme_fivethirtyeight( ), 
	ggplot( ) + geom_histogram( aes( y ) ) + coord_flip( ) + ggtitle( "histo of y" ) + theme_economist( ), 
	ggplot( ) + geom_histogram( aes( x = cut( x = x, breaks = b, labels = l ), y ), stat = "identity", position = "identity" ) + ggtitle( "same kind of histo with position = identity " ) + theme_solarized_2( ),
    cols = 2
)


