load.library <-
    function( name.of.library = NULL ) {
        if( !is.null( name.of.library ) ) {
            if( 0 < i && !require( name.of.library ) ) {
                install.packages( name.of.library )
                stopifnot( require( name.of.library ) )
            }
        }
    }
