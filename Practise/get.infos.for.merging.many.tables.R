get.date <-
    function( d ) { names( d )[ grep( "edat|date|datum", tolower( names( d ) ) ) ] }

get.sic <-
    function( d ) { names( d )[ grep( "sic|pseudonym", tolower( names( d ) ) ) ] }

get.scigroup <-
    function( d ) { names( d )[ grep( "sci_group|sgroup|group|grp|gruppe", tolower( names( d ) ) ) ] }

print.infos.for.merging.many.tables <-
    function( list.of.table.names = ls( ) ) {
        
        for( n in list.of.table.names ) {
            
            tbl <-
                mget( n, .GlobalEnv )[[ 1 ]]
            
            cat( c( "NAME:      ", n, "\n" ) )
            cat( c( "SIC:       ", get.sic( tbl ), "\n" ) )
            cat( c( "SCI_GROUP: ", get.scigroup( tbl ), "\n" ) )
            cat( c( "DATE:      ", get.date( tbl ), "\n" ) )
            cat( c( "VISITS:    ", nrow( tbl ), "\n" ) )
            cat( c( "VISITORS:  ", nrow( unique( tbl[ , get.sic( tbl ) ] ) ), "\n" ) )
            cat( c( "COMPLETE:  ", sum( complete.cases( tbl ) ), "\n" ) )
            cat( "_______________________________\n" ) 
        }
    }

get.infos.for.merging.many.tables <-
    function( list.of.table.names = ls( ) ) {
        
        l <-
            list( )
        
        for( n in list.of.table.names ) {
            
            tbl <-
                mget( n, .GlobalEnv )[[ 1 ]]
            
            l[[ length( l ) + 1 ]] <-
                list( 
                    NAME      = n,
                    SIC       = list( get.sic( tbl ) ),
                    SCI_GROUP = list( get.scigroup( tbl ) ),
                    DATE      = list( get.date( tbl ) ),
                    VISITS    = nrow( tbl ),
                    VISITORS  = nrow( unique( tbl[ , get.sic( tbl ) ] ) ),
                    COMPLETE  = sum( complete.cases( tbl ) ) )
        }
        
        Reduce( bind_rows, l )
    }
