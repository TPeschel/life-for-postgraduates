rm( list = ls( ) )

devtools::install_github( "TPeschel/hlpr4life" )

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

merge.likely(
	d1 = d1,
	d2 = d2,
	by = c( "sic" ),
	by.lk.x = c( "edat", "H" ),
	by.lk.y = c( "edat", "height" ),
	min = c( -11, -4 ),
	max = c( +10, +3 ),
	reorder.names = F,
	trim = F,
	add.diffs = T )

merge.likely(
	d1 = d1,
	d2 = d2,
	by = c( "sic" ),
	by.lk.x = c( "edat", "H" ),
	by.lk.y = c( "edat", "height" ),
	min = c( -11, -4 ),
	max = c( +10, +3 ),
	reorder.names = T,
	trim = F,
	add.diffs = T )

merge.likely(
	d1 = d1,
	d2 = d2,
	by = c( "sic" ),
	by.lk.x = c( "edat", "H" ),
	by.lk.y = c( "edat", "height" ),
	min = c( -11, -4 ),
	max = c( +10, +3 ),
	reorder.names = T,
	trim = T,
	add.diffs = T )

merge.likely(
	d1 = d1,
	d2 = d2,
	by = c( "sic" ),
	by.lk.x = c( "edat", "H" ),
	by.lk.y = c( "edat", "height" ),
	min = c( -11, -4 ),
	max = c( +10, +3 ),
	reorder.names = T,
	trim = T,
	add.diffs = F )

m1 <- 
	merge.likely(
		d1 = d1,
		d2 = d2,
		by = c( "sic" ),
		by.lk.x = c( "edat", "H" ),
		by.lk.y = c( "edat", "height" ),
		min = c( -11, -4 ),
		max = c( +10, +3 ),
		reorder.names = T,
		trim = T,
		add.diffs = F )
m2 <- 
	merge.likely(
		d1 = d2,
		d2 = d1,
		by = c( "sic" ),
		by.lk.x = c( "edat", "height" ),
		by.lk.y = c( "edat", "H" ),
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




d.1 <-
	data.frame(
		a = c( 1 : 10 ),
		b = c( 10 : 1 ),
		d = rep( 0, 10 ),
		x = c( 1 : 10 ),
		y = c( 10 : 1 ),
		z = sample( c( 1 : 5 ), 10, T ) )

d.2 <-
	data.frame(
		a = sample( c( 1 : 10 ), 10 ),
		b = c( 10 : 1 ),
		c = rep( 0, 10 ),
		x = c( 1 : 10 ),
		Y = c( 10 : 1 ),
		Z = sample( c( 1 : 5 ), 10, T ) )

d.1
d.2

( d.fff <-
		merge.likely(
			d1 = d.1,
			d2 = d.2,
			by = c( "a", "b" ),
			by.x = c( "d" ),
			by.y = c( "c" ),
			by.lk = c( "x" ),
			by.lk.x = c( "y", "z" ),
			by.lk.y = c( "Y", "Z" ),
			min = c( -6, -6, -6 ),
			max = c( +5, +5, +5 ),
			reorder.names = F,
			trim = F,
			add.diffs = F ) )
( d.tff <-
		merge.likely(
			d1 = d.1,
			d2 = d.2,
			by = c( "a", "b" ),
			by.x = c( "d" ),
			by.y = c( "c" ),
			by.lk = c( "x" ),
			by.lk.x = c( "y", "z" ),
			by.lk.y = c( "Y", "Z" ),
			min = c( -6, -6, -6 ),
			max = c( +5, +5, +5 ),
			reorder.names = T,
			trim = F,
			add.diffs = F ) )
( d.ftf <-
		merge.likely(
			d1 = d.1,
			d2 = d.2,
			by = c( "a", "b" ),
			by.x = c( "d" ),
			by.y = c( "c" ),
			by.lk = c( "x" ),
			by.lk.x = c( "y", "z" ),
			by.lk.y = c( "Y", "Z" ),
			min = c( -6, -6, -6 ),
			max = c( +5, +5, +5 ),
			reorder.names = F,
			trim = T,
			add.diffs = F ) )
# makes no sense
( d.ttf <-
		merge.likely(
			d1 = d.1,
			d2 = d.2,
			by = c( "a", "b" ),
			by.x = c( "d" ),
			by.y = c( "c" ),
			by.lk = c( "x" ),
			by.lk.x = c( "y", "z" ),
			by.lk.y = c( "Y", "Z" ),
			min = c( -6, -6, -6 ),
			max = c( +5, +5, +5 ),
			reorder.names = T,
			trim = T,
			add.diffs = F ) )
( d.fft <-
		merge.likely(
			d1 = d.1,
			d2 = d.2,
			by = c( "a", "b" ),
			by.x = c( "d" ),
			by.y = c( "c" ),
			by.lk = c( "x" ),
			by.lk.x = c( "y", "z" ),
			by.lk.y = c( "Y", "Z" ),
			min = c( -6, -6, -6 ),
			max = c( +5, +5, +5 ),
			reorder.names = F,
			trim = F,
			add.diffs = T ) )
( d.tft <-
		merge.likely(
			d1 = d.1,
			d2 = d.2,
			by = c( "a", "b" ),
			by.x = c( "d" ),
			by.y = c( "c" ),
			by.lk = c( "x" ),
			by.lk.x = c( "y", "z" ),
			by.lk.y = c( "Y", "Z" ),
			min = c( -6, -6, -6 ),
			max = c( +5, +5, +5 ),
			reorder.names = T,
			trim = F,
			add.diffs = T ) )
( d.ftt <-
		merge.likely(
			d1 = d.1,
			d2 = d.2,
			by = c( "a", "b" ),
			by.x = c( "d" ),
			by.y = c( "c" ),
			by.lk = c( "x" ),
			by.lk.x = c( "y", "z" ),
			by.lk.y = c( "Y", "Z" ),
			min = c( -6, -6, -6 ),
			max = c( +5, +5, +5 ),
			reorder.names = F,
			trim = T,
			add.diffs = T ) )
( d.ttt <-
		merge.likely(
			d1 = d.1,
			d2 = d.2,
			by = c( "a", "b" ),
			by.x = c( "d" ),
			by.y = c( "c" ),
			by.lk = c( "x" ),
			by.lk.x = c( "y", "z" ),
			by.lk.y = c( "Y", "Z" ),
			min = c( -6, -6, -6 ),
			max = c( +5, +5, +5 ),
			reorder.names = T,
			trim = T,
			add.diffs = T ) )

