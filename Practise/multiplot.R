rm( list = ls( ) )
x <- rnorm( 50, 10, 2 )
y <- 1 * x + rnorm( 50, 5, 2 )

library( life.helper )

b <- seq( floor( min( x ) ), ceiling( max( x ) ), by = .5 )
l <- b[ -1 ] - .25
length(b)
length(l)

ggplot( ) + geom_histogram( aes( x = cut( x = x, breaks = b, labels = l ), y ), stat = "identity", position = "identity" )

multiplot( 
    ggplot( ) + geom_histogram( aes( y ) ), 
    ggplot( ) + geom_histogram( aes( x ) ), 
    ggplot( ) + geom_point( aes( x, y ) ),
    ggplot( ) + geom_histogram( aes( x = cut( x = x, breaks = b, labels = l ), y ), stat = "identity", position = "identity" ),
    ggplot( ) + geom_histogram( aes( x = cut( x = x, breaks = b, labels = l ), y ), stat = "identity" ),
    cols = 2
)

