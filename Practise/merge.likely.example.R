rm( list = ls( ) )

library( hlpr4life )

load.pkgs( c( "ggplot2", "lubridate", "dplyr" ) )

Sys.time( )
Sys.timezone( )
Sys.setenv( TZ = "BMT" )

clndr <-
	calendar( "1973-08-08", "2017-06-19" )

edats <-
	calendar( "2010-01-01", "2017-06-19" )

d1 <-
	data.frame(
		sic  = paste0( "LI00", sample( make.sics( n.first = 9991, n.last = 10020 ), 300, T ) ),
		edat = as.Date( sample( edats$date, 300, T ) ),
		H    = round( rnorm( 300, 180, 10 ) ))

d2 <-
	data.frame(
		sic = paste0( "LI00", sample( make.sics( n.first = 9991, n.last = 10020 ), 300, T ) ),
		edat = as.Date( sample( edats$date, 300, T ) ),
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
