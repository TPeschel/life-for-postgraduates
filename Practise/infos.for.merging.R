hlpr4life::load.pkgs(
	c( 
		"hlpr4life",
		"ggplot2",
		"reshape2",
		"dplyr" )
)

get.columns.of.date <-
	function( d ) { names( d )[ grep( "edat|date|datum", tolower( names( d ) ) ) ] }

get.columns.of.sic <-
	function( d ) { names( d )[ grep( "sic|pseudonym", tolower( names( d ) ) ) ] }

get.columns.of.scigroup <-
	function( d ) { names( d )[ grep( "sci_group|sgroup|group|grp|gruppe", tolower( names( d ) ) ) ] }

print.infos.for.merging.many.tables <-
	function( list.of.table.names = ls( ) ) {
		
		for( n in list.of.table.names ) {
			
			tbl <-
				as.data.frame( mget( n, .GlobalEnv )[[ 1 ]] )
			
			cat( c( "NAME:      ", n, "\n" ) )
			cat( c( "SIC:       ", get.columns.of.sic( tbl ), "\n" ) )
			cat( c( "SCI_GROUP: ", get.columns.of.scigroup( tbl ), "\n" ) )
			cat( c( "DATE:      ", get.columns.of.date( tbl ), "\n" ) )
			cat( c( "VISITS:    ", nrow( tbl ), "\n" ) )
			cat( c( "VISITORS:  ", length( unique( tbl[ , get.columns.of.sic( tbl ) ] ) ), "\n" ) )
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
				as.data.frame( mget( n, .GlobalEnv )[[ 1 ]] )
			
			l[[ length( l ) + 1 ]] <-
				list( 
					NAME      = n,
					SIC       = list( get.columns.of.sic( tbl ) ),
					SCI_GROUP = list( get.columns.of.scigroup( tbl ) ),
					DATE      = list( get.columns.of.date( tbl ) ),
					VISITS    = nrow( tbl ),
					VISITORS  = length( unique( tbl[ , get.columns.of.sic( tbl ) ] ) ),
					COMPLETE  = sum( complete.cases( tbl ) ) )
		}
		
		Reduce( bind_rows, l )
	}

##NAs anzeigen
show.nas <-
	function( data ) sapply( data, function( col ) sum( is.na( col ) ) )

##not NAs anzeigen
show.avs <-
	function( data ) sapply( data, function( col ) sum( !is.na( col ) ) )

# loesche eine Spalte aus einem Dataframe
delete.column <-
	function( tbl, column.name ) tbl[ , column.name != names( tbl ) ]

# loesche mehrere Spalten aus einem Dataframe
delete.columns <-
	function( tbl, column.names ) tbl[ , !names( tbl ) %in% column.names ]

plot.follow.ups <-
	function( 
		data, 
		bins.days = 7, 
		cohorts.names  = c( "3. A2", "2. A2 School-Classes", "1. B1" ),
		cohorts.prefix = c( "A2_", "A2-SK", "B1" ),
		cohorts.colors = c( "red", "green", "blue" ) ) {
		
		tbl <-
			data
		
		tbl <-
			as.data.frame( tbl )
		
		
		sic.name <-
			get.columns.of.sic( tbl )[ 1 ]
		
		sci.group.name <-
			get.columns.of.scigroup( tbl )[ 1 ]
		
		edat.name <-
			get.columns.of.date( tbl )[ 1 ]
		
		cols <-
			sort( paste0( tbl[[ sic.name ]], ":", as.POSIXct( tbl[[ edat.name ]] ) ) )
		
		tc <-
			tbl[ match( cols, paste0( tbl[[ sic.name ]], ":", as.POSIXct( tbl[[ edat.name ]] ) ) ), ]
		
		grp <-
			sapply( 
				cohorts.prefix,
				function( ko ) grep( ko, tc[[ sci.group.name ]] ) )
		
		if( !is.list( grp ) ) {
			
			n <- 
				dimnames( grp )[[ 2 ]]
			
			grp <-
				list(
					value = c( 1 : length( grp ) ),
					L1    = rep( n, length( grp ) ) )
		}
		
		grp <-
			melt( grp ) 
		
		grp$L1 <-
			cohorts.names[ match( grp$L1, cohorts.prefix ) ]
		
		tc$GRP[ grp$value ] <-
			grp$L1
		
		tc$FOLLOW.UP <- 
			as.POSIXct( c( tc[ 2 : nrow( tc ), edat.name ], NA ) )
		
		tc$FOLLOW.UP.SIC <-
			c( tc[ 2 : nrow( tc ), sic.name ], NA )
		
		tc <-
			tc[ tc[[ sic.name ]] == tc$FOLLOW.UP.SIC, ]
		
		tc <- 
			tc[ -1, ]
		
		if( nrow( tc ) < 1 ) {
			
			return( ggplot( ) + theme_bw( ) )
		}
		tc$FOLLOW.UP.DIFF.TIME <-
			as.numeric( difftime( tc$FOLLOW.UP, tc[[ edat.name ]], units = "day" ) )
		
		tc <-
			tc[ !is.na( tc$FOLLOW.UP.DIFF.TIME ), ]

		tc$FOLLOW.UP.DIFF.TIME.CAT <-
			cut( 
				tc$FOLLOW.UP.DIFF.TIME,
				seq( 0, max( tc$FOLLOW.UP.DIFF.TIME, na.rm = T ), by = bins.days ),
				seq( bins.days, max( tc$FOLLOW.UP.DIFF.TIME, na.rm = T ), by = bins.days ) / bins.days )
		
		ggplot( tc ) +
			theme_bw( ) +
			ggtitle( deparse( substitute( data ) ) ) +
			geom_histogram( 
				aes( 
					FOLLOW.UP.DIFF.TIME.CAT, 
					#fill = c( "1. school class", "2. private visit" )[ match( grepl( "SK", tc[ , sci.group.name ] ), c( T, F ) ) ] ), stat = "count" ) +
					#fill = tc[ , sci.group.name ] ), stat = "count" ) +
					fill = tc[ , "GRP" ] ), stat = "count" ) +
			theme( 
				axis.text.x = element_text( angle = 90, size = 7 ),
				#		panel.grid = element_blank( ),
				panel.grid.minor.x = element_blank( ),
				panel.grid.major.x = element_blank( ),
				legend.position = "top" ) +
			scale_x_discrete( name = paste0( "x ", bins.days, " day(s) between 2 next visits" ), limits = c( 1 : ( 1 + ( max( tc$FOLLOW.UP.DIFF.TIME, na.rm = T ) ) / bins.days ) ) ) +
			scale_fill_manual( name = "", values = cohorts.colors )
	}

