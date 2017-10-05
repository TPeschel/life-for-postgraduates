rm( list = ls( ) )
## hier drin sind alle funktionen
source( "infos.for.merging.R" )

# lade einfach mal ein paar tabellen
d040 <-
	readxl::read_excel( "../LeaOelkers/2017.20.23/data/original/PV0298_D00040_NODUP.xlsx" )

d077 <-
	readxl::read_excel( "../LeaOelkers/2017.20.23/data/original/PV0298_D00077_NODUP.xlsx" )

d127 <-
	readxl::read_excel( "../LeaOelkers/2017.20.23/data/original/PV0298_D00127_NODUP.xlsx" )

t109 <-
	readxl::read_excel( "../LeaOelkers/2017.20.23/data/original/PV0298_T00109_NODUP.xlsx" )

name.of.data.frame( t109 )

get.columns.of.sic( d127 )
get.columns.of.scigroup( d127 )
get.columns.of.date( d127 )

( table.names <-
	ls( )[ grep( "(d|t)[0-9]{3}", ls( ) ) ] )

( infos <-
	get.infos.for.merging.many.tables( table.names ) )

infos$NAME
infos$SIC
infos$SCI_GROUP
infos$DATE

print.infos.for.merging.many.tables( table.names )

## zeige alle availablen
show.avs( d040 )

## zeige alle missings
show.nas( d040 )

get.columns.of.date( d127 )

d127 <-
	delete.column( d127, "C_DISEASE_TX_MAX_EDAT" )

get.columns.of.date( d127 )

names( d127 )

( names.with.neuro.or.kardio <-
	names( d127 )[ grepl( "NEURO|KARDIO", names( d127 ) ) ] )

d127 <-
	delete.columns( d127, names.with.neuro.or.kardio )

names( d127 )

ggsubplot(
	plot.follow.ups( d040, 365.25 / 12 ),
	plot.follow.ups( d077, 365.25 / 12 ),
	plot.follow.ups( d127, 365.25 / 12 ),
	cols = 1 )

d040.d077 <- 
	merge( 
		d040, 
		d077, 
		by.x = c( 
			get.columns.of.sic( d040 )[ 1 ], 
			get.columns.of.scigroup( d040 )[ 1 ] ),
		by.y = c(
			get.columns.of.sic( d077 )[ 1 ], 
			get.columns.of.scigroup( d077 )[ 1 ] ) )

ggsubplot(
	plot.follow.ups( d040, 7 ),
	plot.follow.ups( d077, 14 ),
	plot.follow.ups( d127, 28 ),
	plot.follow.ups( d040.d077, 365.25/12 ),
	plot.follow.ups( t109, 7 ),
	plot.follow.ups( 
		t109, 
		365.25/12 ),
	cols = 2 )

plot.follow.ups( 
	t109, 
	7,
	cohorts.names  = unique( t109[[ get.columns.of.scigroup( t109 ) ]] ),
	cohorts.prefix = unique( t109[[ get.columns.of.scigroup( t109 ) ]] ),
	cohorts.colors = sample( colors( ), length( unique( t109[[ get.columns.of.scigroup( t109 ) ]] ) ) ) )

plot.follow.ups( 
	d077, 
	7,
	cohorts.names  = unique( d077[[ get.columns.of.scigroup( d077 ) ]] ),
	cohorts.prefix = unique( d077[[ get.columns.of.scigroup( d077 ) ]] ),
	cohorts.colors = c( 
		as.character( color.gradient( "red", "black", 13 )[["c"]] ),
		as.character( color.gradient( "green", "blue", 5 )[["c"]] ),
		as.character( color.gradient( "yellow", "brown", 12 )[["c"]] ) ) )

d077$months <-
	c( "01/Jan", "02/Feb", "03/Mar", "04/Apr", "05/May", "06/Jun", "07/Jul", "08/Aug", "09/Sep", "10/Oct", "11/Nov", "12/Dec" )[
		match( lubridate::month( as.Date( d077$C_PUB_STAT_EDAT ) ), c( 1 : 12 ) ) ]

d077$GRP.cohort.month <-
	paste0( d077$C_PUB_STAT_GRUPPE, ":", d077$months )

unique( d077$GRP.cohort.month )

get.columns.of.scigroup( d077 )

d077 <-
	rename.column( d077, "C_PUB_STAT_GRUPPE", "G.R.U.P.P.E" )

get.columns.of.scigroup( d077 )

plot.follow.ups( 
	d077, 
	365.25,
	cohorts.names  = unique( d077$GRP.cohort.month ),
	cohorts.prefix = unique( d077$GRP.cohort.month ),
	cohorts.colors = colors( ) ) + 
	theme( legend.position = c( -2.7, -2.2 ) ) +
	facet_grid( months ~ G.R.U.P.P.E  )

d077$SEX <-
	c( "female", "male" )[ match( d077$C_PUB_STAT_GENDER, c( 2, 1 ) ) ]

table( d077$months, d077$SEX )

plot.follow.ups( 
	d077, 
	28  ) +
	facet_grid( months ~ SEX  )

ggsubplot(
	plot.follow.ups( 
		d077, 
		28  ) +
		facet_grid( months ~ SEX  ),
	plot.follow.ups( 
		d077, 
		365.25 / 12  ) +
		facet_grid( months ~ SEX  ),
	plot.follow.ups( 
		d077, 
		14  ) +
		facet_grid( months ~ SEX ),
	layout = 
		t(
			matrix(
				c(
					1, 2,
					3, 3
				),
				nrow = 2 ) ) )

ggsubplot(
	plot.follow.ups( 
		d077, 
		28  ) +
		facet_grid( months ~ SEX  ),
	plot.follow.ups( 
		d077, 
		365.25 / 12  ) +
		facet_grid( months ~ SEX  ),
	plot.follow.ups( 
		d077, 
		14  ) +
		facet_grid( months ~ SEX ),
	layout = 
		t(
			matrix(
				c(
					1, 3,
					2, 3
				),
				nrow = 2 ) ) )

df.x <-
	d077

df.y <-
	d127


d <-
	merge( df.x, df.y, by.x = get.columns.of.sic( df.x )[ 1 ], by.y = get.columns.of.sic( df.y )[ 1 ] )

d$DT <-
	as.numeric( difftime( d[[ get.columns.of.date( df.x ) ]], d[[ get.columns.of.date( df.y ) ]], units = "day" ) )

print.infos.for.merging.many.tables( c( "df.x", "df.y" ) )

hist( d$DT )
table( d$DT )

d <-
	d[ abs( d$DT ) < 365.25 / 2, ]

d.1 <-
	d[ abs( d$DT ) < 1, ]

hist( d.1$DT )

table( d.1$DT )
table( d.1$DT * 24 )

d.1$DTC <-
	cut( d.1$DT, breaks = 24 )
	
ggplot( d.1 ) + geom_histogram( aes( DTC ), stat = "count" )
