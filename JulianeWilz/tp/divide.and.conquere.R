rm( list = ls( ) )

d.a.q <-
    function( x, inner.fct = sum, outer.fct = sum ) {
        
        if( length( x ) == 1 ) {
            
            return( inner.fct( x ) )
        }
        
        md <-
            floor( ( 1 + length( x ) ) / 2 )
        
        outer.fct(
            d.a.q( x[ c( 1 : md ) ], inner.fct, outer.fct ),
            d.a.q( x[ c( ( md + 1 ) : length( x ) ) ], inner.fct, outer.fct ) ) }

x <-
    rnorm( 100 )

x <- 
    c( 1:16 )

d.a.q(
    x,
    sum,
    function( a, b ) {
        sd( c( a, b ) ) / ( length( a ) + length( b ) ) } )


d.a.q(
    x,
    sum,
    function( a, b ) {
        a + b } )
d.a.q(
    x,
    sum,
    sum )

mean( x )

sum( 1:2)
x <- c( 1 : 2 )
fct <- sum

