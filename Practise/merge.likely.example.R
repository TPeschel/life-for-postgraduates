rm( list = ls( ) )

library( hlpr4life )

load.pkgs( c( "ggplot2", "lubridate", "dplyr" ) )

Sys.time( )
Sys.timezone( )
Sys.setenv( TZ = "BMT" )

calendar <-
	function( start = "2010-01-01 00:00:00", end = "2017-06-19 00:00:00" ) as.POSIXct( seq( as_datetime( start ), as_datetime( end ), by = 24 * 3600 ), tz = "BMT" )

clndr <-
	calendar( "2010-01-01", "2017-06-19" )

week.days <-
	clndr[ wday( clndr ) %in% c( 1 : 5 ) ]

make.sics <-
	function( n ) {
		sc <-
			sapply(
				as.character( c( 1 : n ) ),
				function( s ) {
					paste0(
						paste(
							rep(
								"0",
								times = floor( log10( n ) + 1 ) - nchar( s ) ),
							collapse = "" ),
						s ) } )
		names( sc ) <-
			NULL
		sc }

d1 <-
	data.frame(
		sic  = sample( make.sics( 30 ), 300, T ),
		edat = as.Date( sample( clndr, 300 ) ),
		H    = round( rnorm( 300, 180, 10 ) ))

d2 <-
	data.frame(
		sic = sample( make.sics( 30 ), 300, T ),
		edat = as.Date( sample( clndr, 300 ) ),
		height = round( rnorm( 300, 180, 10 ) ) )

ml(
	d1 = d1,
	d2 = d2,
	by = c( "sic" ),
	by.x.lk = c( "edat", "H" ),
	by.y.lk = c( "edat", "height" ),
	min = c( -11, -4 ),
	max = c( +10, +3 ),
	reorder.names = F,
	trim = F,
	add.diffs = T )

ml(
	d1 = d1,
	d2 = d2,
	by = c( "sic" ),
	by.x.lk = c( "edat", "H" ),
	by.y.lk = c( "edat", "height" ),
	min = c( -11, -4 ),
	max = c( +10, +3 ),
	reorder.names = T,
	trim = F,
	add.diffs = T )

ml(
	d1 = d1,
	d2 = d2,
	by = c( "sic" ),
	by.x.lk = c( "edat", "H" ),
	by.y.lk = c( "edat", "height" ),
	min = c( -11, -4 ),
	max = c( +10, +3 ),
	reorder.names = T,
	trim = T,
	add.diffs = T )

ml(
	d1 = d1,
	d2 = d2,
	by = c( "sic" ),
	by.x.lk = c( "edat", "H" ),
	by.y.lk = c( "edat", "height" ),
	min = c( -11, -4 ),
	max = c( +10, +3 ),
	reorder.names = T,
	trim = T,
	add.diffs = F )

m1 <- 
	ml(
		d1 = d1,
		d2 = d2,
		by = c( "sic" ),
		by.x.lk = c( "edat", "H" ),
		by.y.lk = c( "edat", "height" ),
		min = c( -11, -4 ),
		max = c( +10, +3 ),
		reorder.names = T,
		trim = T,
		add.diffs = F )
m2 <- 
	ml(
		d1 = d2,
		d2 = d1,
		by = c( "sic" ),
		by.x.lk = c( "edat", "height" ),
		by.y.lk = c( "edat", "H" ),
		min = c( -11, -4 ),
		max = c( +10, +3 ),
		reorder.names = T,
		trim = T,
		add.diffs = F )
m1
m2

m1[ , -sapply( m1, is.factor ) ] - m2[ , -sapply( m2, is.factor ) ]

cbind( 
	m1,
	m2[ , -sapply( m2, is.factor ) ] - m1[ , -sapply( m1, is.factor ) ] )
