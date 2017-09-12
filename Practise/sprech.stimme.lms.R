rm( list = ls( ) )

ifnot <-
	function( cond, optTrue, optFalse = { } ) {
		
		if( !cond ) {
			
			optTrue 
			
		} else {
			
			optFalse } }
	
# ifnot( 
# 	"devtools" %in% rownames( installed.packages( ) ), 
# 	{install.packages( "devtools" );library( devtools ) },
# 	library( devtools ) )
# 
# install_github( "TPeschel/hlpr4life" )

hlpr4life::load.pkgs(
	c(
		"hlpr4life",
		"ggplot2",
		"reshape2",
		"dplyr",
		"gamlss" ) )

source( "get.mu.nu.sigma.tau.R" )

load( "/home/tpeschel/LIFE/life-for-postgraduates/ThomasBerger/r/data_sprech.Rda" )

d <-
	data.sprech

rm( "data.sprech" )

my.thm <-
	list(
		theme_bw( ),
		scale_color_manual( guide = F, values = c( "deeppink", "deepskyblue" ) ),
		scale_fill_manual( guide = F, values = c( "deeppink", "deepskyblue" ) ),
		facet_grid( sex ~ . ) )

ggplot( d, aes( age, sprech4_f0_sds, col = sex ) ) +
	my.thm +
	geom_point( ) +
	geom_smooth( )

ggplot( d, aes( age, F0_SPRECH_4, col = sex ) ) +
	my.thm +
	geom_point( ) +
	geom_smooth( )

d <-
	rename.columns(
		na.omit(
			d[ , c( "SIC", "SGROUP.x", "sex", "age", "FAM_ID", names( d )[ grep( "F0_SPRECH_", names( d ) ) ], names( d )[ grep( "sprech", names( d ) ) ] ) ] ),
		c( "SIC", "SGROUP.x", "FAM_ID", names( d )[ grep( "F0_SPRECH_", names( d ) ) ], names( d )[ grep( "sprech", names( d ) ) ] ),
		c( "id", "edat", "fam", paste0( "f0.", c( 1 : 5 ) ), paste0( "f0.", c( 1 : 5 ), ".sds" ) ) )

d <-
	arrange( 
		d,
		sex, age, fam, id )

d %<>%
	group_by( id ) %>%
	mutate( w.edat = n( ) )

d %<>%
	group_by( fam ) %>%
	mutate( w.fam = length( unique( id ) ) )

d$weight <-
	1 / ( d$w.edat * d$w.fam )

head( d )

# ggsubplot(
# 	ggplot( d, aes( age, weight, col = sex ) ) +
# 		my.thm +
# 		geom_point( ),
# 	ggplot( d, aes( age, d$w.edat, col = sex ) ) +
# 		my.thm +
# 		geom_point( ),
# 	ggplot( d, aes( age, d$w.fam, col = sex ) ) +
# 		my.thm +
# 		geom_point( ),
# 	cols = 3 )
# 
# ggsubplot(
# 	ggplot( d, aes( age, weight, col = sex ) ) +
# 		my.thm +
# 		geom_point( ),
# 	ggplot( d, aes( age, w.edat, col = sex ) ) +
# 		my.thm + facet_grid( sex ~ weight ) +
# 		geom_point( ),
# 	ggplot( d, aes( age, w.fam, col = sex ) ) +
# 		my.thm + facet_grid( sex ~ weight ) +
# 		geom_point( ),
# 	ggplot( d, aes( weight, fill = sex ) ) +
# 		my.thm + facet_grid( w.fam ~ w.edat ) +
# 		geom_histogram( ),
# 	cols = 2 )

d. <- 
	d[ , c( "id", "age", "sex", "fam", "weight", "w.edat", "w.fam" ) ]

arrange( d.[ d.$w.edat==1 & d.$w.fam == 2,], id )

ggplot( d., aes( weight, fill = sex ) ) +
	my.thm + facet_grid( w.fam ~ w.edat, scales = "free" ) +
	geom_histogram( position = "dodge" ) +
	scale_x_continuous( breaks = round( 2 / c( 2 : 10 ), 2 ), labels = c( "1", paste0( "2/", c( 3 : 10 ) ) ) ) +
	coord_flip( )

s <-
	rename.columns(
		as.data.frame(
			table( d$w.fam, d$w.edat ) ),
		c( "Var1", "Var2" ),
		c( "family size", "visits" ) )

ggplot( s ) + theme_classic( ) +
	geom_point( aes( `family size`, visits, size = Freq ), col = "green", fill = "yellow", shape = 5 ) +
	geom_text( aes( `family size`, visits, label = Freq ) )

s <-
	rename.columns(
		as.data.frame(
			round( prop.table( table( d$w.fam, d$w.edat ) ), 4 ) ),
		c( "Var1", "Var2" ),
		c( "family size", "visits" ) )

s$Freq <-
	paste0(
		100 * round( s$Freq, 4 ), "%" )

ggplot( s ) + theme_classic( ) +
	geom_text( aes( `family size`, visits, label = Freq ) )

# d.f <-
# 	d[ d$sex == "female", ]

# d.f$unique <-
# 	c( 1 : nrow( d.f ) )

# d.m <-
# 	d[ d$sex == "male", ]

# d.m$unique <-
# 	c( 1 : nrow( d.m ) )

# d.m.msnt.f0.1 <-
# 	lms( y = f0.1, x = age, families = "BCT", data = d.m, k = 2 )
# 
# d.f.msnt.f0.1 <-
# 	lms( y = f0.1, x = age, families = "BCT", data = d.f, k = 2 )
# 
# d.m.msnt.f0.4 <-
# 	lms( y = f0.4, x = age, families = "BCCGo", data = d.m, k = 2 )
# 
# d.f.msnt.f0.4 <-
# 	lms( y = f0.4, x = age, families = "BCPEo", data = d.f, k = 2 )

ages <-
	seq( floor( min( d$age ) ), ceiling( max( d$age ) ), by = 1 / 12 )

age <-
	cut( d$age, ages )

sttstc.f.lst <-
	list( )

sttstc.m.lst <-
	list( )

lms.f.lst <-
	list( )

lms.m.lst <-
	list( )

for( loop in c( 1 : 1 ) ) {
	
	print( paste0( "loop: ", loop ) )
	
	res <-
		get.msnt(
			rename.columns(
				d[ d$sex == "female", ],
				c( "f0.4" ),
				c( "val" ) ),
			ages,
			"BCPEo",
			.75 )
	
	if( !is.null( res ) ) {
		
		sttstc.f.lst[[ length( sttstc.f.lst ) + 1 ]] <-
			res$statistic
		
		res$lms$loop <-
			rep( length( lms.f.lst ) + 1, length( ages ) )
		
		lms.f.lst[[ length( lms.f.lst ) + 1 ]] <-
			res$lms
		
		print( paste0( "female successes: ", length( lms.f.lst ) ) ) } 
	
	res <-
		get.msnt(
			rename.columns(
				d[ d$sex == "male", ],
				c( "f0.4" ),
				c( "val" ) ),
			ages,
			"BCCGo",
			.75 )
	
	if( !is.null( res ) ) { 
		
		sttstc.m.lst[[ length( sttstc.m.lst ) + 1 ]] <-
			res$statistic
		
		res$lms$loop <-
			rep( length( lms.m.lst ) + 1, length( ages ) )

		lms.m.lst[[ length( lms.m.lst ) + 1 ]] <-
			res$lms
		
		print( paste0( "male successes: ", length( lms.m.lst ) ) ) } }


( st.f <-
	Reduce( bind_rows, sttstc.f.lst ) )

colSums( st.f, na.rm = T )

( st.m <-
	Reduce( bind_rows, sttstc.m.lst ) )

colSums( st.f, na.rm = T )
	
summary( st.f )
summary( st.m )

lms.f. <-
	melt( Reduce( bind_rows, lms.f.lst ), c( "age", "loop" ) ) 

lms.m. <-
	melt( Reduce( bind_rows, lms.m.lst ), c( "age", "loop" ) ) 

lms.f.$sex <-
	"female"

lms.m.$sex <-
	"male"

lms.f.$family <-
	"BCPEo"

lms.m.$family <-
	"BCCGo"

l <-
	as.data.frame( rbind( lms.f., lms.m. ) )

l. <-
	l %>%
		group_by( sex, family, age, variable ) %>%
		summarise( MEAN.VAL = mean( value ) )

ggsubplot(
	ggplot( ) + 
		my.thm + 
		scale_color_manual( values = c( "female" = "deeppink", "male" = "deepskyblue" ), guide = F ) +
		scale_x_continuous( breaks = c( 0 : 20 ) ) +
		geom_point( aes( age, f0.4, col = sex ), d ) +
		geom_smooth( aes( age, f0.4, col = sex ), d, method = "loess" ) +
		geom_line( aes( age, MEAN.VAL, group = sex ), l.[l.$variable=="mu",], col = "black" ),
	ggplot( ) +
		my.thm +
		geom_line( aes( age, value, col = sex, group = paste0( sex, loop ) ), l,alpha = .2 ) +
		facet_grid( variable ~ ., scales = "free" ) +
		scale_x_continuous( breaks = c( 0 : 20 ) ) +
		theme( axis.text.x = element_text( angle = 90 ) ) +
		geom_line( aes( age, MEAN.VAL, group = sex ), l., col = "black" ),
	cols = 2 )

#save( "l.", file = "lms.D75.N175.2.Rd" )

# load( "lms.100.Rd" )
# l100. <-
# 	l.
# load( "lms.50.Rd" )
# l50. <-
# 	l.
# load( "lms.20.Rd" )
# l20. <-
# 	l.
# load( "lms.50.b.Rd" )
# l50b. <-
# 	l.
# ggplot( ) +
# 	my.thm +
# 	facet_grid( variable ~ sex, scales = "free" ) +
# 	scale_x_continuous( breaks = c( 0 : 20 ) ) +
# 	geom_line( aes( age, MEAN.VAL, group = sex ), l20., col = "green" ) +
# 	geom_line( aes( age, MEAN.VAL, group = sex ), l50., col = "red" ) +
# 	geom_line( aes( age, MEAN.VAL, group = sex ), l50b., col = "yellow" ) +
# 	geom_line( aes( age, MEAN.VAL, group = sex ), l100., col = "blue" )
# 
# load( "lms.D75.N175.Rd" )
# 
# lms.D75.N175 <-
# 	l.
# 
# load( "lms.D75.N175.2.Rd" )
# 
# lms.D75.N175.2 <-
# 	l.
# 
# ggplot( ) +
# 	my.thm +
# 	facet_grid( variable ~ sex, scales = "free" ) +
# 	scale_x_continuous( breaks = c( 0 : 20 ) ) +
# 	geom_line( aes( age, MEAN.VAL, group = sex ), lms.D75.N175, col = "green" ) +
# 	geom_line( aes( age, MEAN.VAL, group = sex ), lms.D75.N175.2, col = "blue" )
# 
# 
