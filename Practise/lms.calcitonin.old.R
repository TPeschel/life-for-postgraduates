#! usr/bin/env Rscript

rm( list = ls( ) )

ifnot <-
	function( cond, optTrue, optFalse = { } ) {
		
		if( !cond ) {
			
			optTrue 
			
		} else {
			
			optFalse } }

list.append <-
	function( lst, x ) {
	
	lst[[ length( lst ) + 1 ]] <-
		x
	
	lst }

ifnot(
	"devtools" %in% rownames( installed.packages( ) ),
	{install.packages( "devtools" );library( devtools ) },
	library( devtools ) )

install_github( "TPeschel/hlpr4life" )

hlpr4life::load.pkgs(
	c(
		"hlpr4life",
		"ggplot2",
		"reshape2",
		"dplyr",
		"gamlss",
		"readxl" ) )

source( "get.mu.nu.sigma.tau.R" )

# d1 <-
# 	read_excel( "~/LIFE/life-for-postgraduates/JulianeWilz/data/AktuelleTabelle220517excel.xlsx" )

d <-
	read_excel( "/home/tpeschel/LIFE/life-for-postgraduates/JulianeWilz/sent/data.2017.08.31/AktuelleTabelle190517excel.xlsx" )

# nt <-
# 	read_excel( "~/LIFE/life-for-postgraduates/JulianeWilz/data/NeueTabelle170517excel.xlsx" )
# 
# nf <-
# 	read_excel( "~/LIFE/life-for-postgraduates/JulianeWilz/data/Nach_Filterung230417.xlsx" )

my.thm <-
	list(
		theme_bw( ),
		scale_color_manual( guide = F, values = c( "deeppink4", "deepskyblue4" ) ),
		scale_fill_manual( guide = F, values = c( "orange", "royalblue" ) ),
		facet_grid( SEX ~ . ),
		theme( axis.text.x = element_text( angle = 90, size = 7 ) ) )


colSums( is.na( d[ , grep( "AGE", names( d ) ) ] ) )

orig.col.names <-
	c(
		"CT_S_1_SIC",
		"CT_S_1_GRUPPE",
		"TEILNEHMER_GESCHLECHT",
		"AGE_Calcitionin",
		"CT_S_1_NUM_VALUE",
		"C_ANTHRO_KH_HEIGHT_ORIG",
		"C_ANTHRO_KH_HEIGHT_ADJ",
		"C_ANTHRO_KH_WEIGHT_ORIG",
		"C_ANTHRO_KH_WEIGHT_ADJ",
		"C_ANTHRO_KH_BMI_ORIG",
		"C_ANTHRO_KH_BMI_ADJ" )

new.col.names <-
	c(
		"SIC",
		"GRUPPE",
		"SEX",
		"AGE",
		"CALCITONIN",
		"HEIGHT",
		"HEIGHT.ADJ",
		"WEIGHT",
		"WEIGHT.ADJ",
		"BMI",
		"BMI.ADJ" )

d <-
	na.omit(
		rename.columns( 
			d[ , orig.col.names ],
			orig.col.names,
			new.col.names ) )

d$AGE <-
	as.numeric( as.character( d$AGE ) )

range( d$AGE )

ages <-
	seq( 0, ceiling( max( d$AGE ) ), by = 1 / 12 )

ages.labels <-
	ages[ -length( ages ) ]

ages.labels <-
	paste0(
		floor( ages.labels ),
		":",
		1 + round( ( ages.labels - floor( ages.labels ) ) * 12.001 ) )

d$AGE.BINS <-
	cut(
		d$AGE,
		breaks = ages,
		labels = ages.labels )

d$LOG10.CALCITONIN <-
	log10( d$CALCITONIN )

d %<>%
	group_by( SIC, GRUPPE ) %>%
	mutate( VISITS = n( ), WGHT = 1 / VISITS )

ggplot( d, aes( AGE, CALCITONIN, col = SEX ) ) +
	my.thm +
	geom_point( alpha = .1 ) +
	geom_smooth( col = "black" )

ggplot( d, aes( AGE.BINS, CALCITONIN, col = SEX ) ) +
	my.thm +
	geom_point( alpha = .1 ) +
	geom_smooth( col = "black" )

d.m <-
	rename.columns(
		d[ d$SEX == "male", ],
		c( "AGE", "SEX", "WGHT", "CALCITONIN" ),
		c( "age", "sex", "weight", "val" ) )

d.f <-
	rename.columns(
		d[ d$SEX == "female", ],
		c( "AGE", "SEX", "WGHT", "CALCITONIN" ),
		c( "age", "sex", "weight", "val" ) )

list.LMS.m <-
	list( )

list.LMS.f <-
	list( )

list.STAT.m <-
	list( )

list.STAT.f <-
	list( )

for( loop in c( 1 : 1000 ) ) {
	
	print( paste0( "Loop: ", loop ) )
	
	print( "fit males" )
	
	MSNT.m <-
		get.msnt( 
			d.m,
			ages = ages,
			family = "BCTo",
			1 )
	
	if( !is.null( MSNT.m ) ) {
		
		MSNT.m$LMS$loop <-
			rep( length( list.LMS.m ) + 1, length( ages ) )
		
		list.LMS.m <-
			list.append( list.LMS.m, MSNT.m$LMS )
		
		list.STAT.m <-
			list.append( list.STAT.m, MSNT.m$STAT ) 
		
		print( paste0( "males fit success: ", length( list.LMS.m ) ) ) }
	
	print( "fit females" )
	
	MSNT.f <-
		get.msnt( 
			d.f,
			ages = ages,
			family = "BCTo",
			1 )
	
	if( !is.null( MSNT.f ) ) {
		
		MSNT.f$LMS$loop <-
			rep( length( list.LMS.f ) + 1, length( ages ) )
		
		list.LMS.f <-
			list.append( list.LMS.f, MSNT.f$LMS )
	
		list.STAT.f <-
			list.append( list.STAT.f, MSNT.f$STAT ) 
		
		print( paste0( "females fit success: ", length( list.LMS.f ) ) ) } }


( 
	st.f <-
		Reduce( bind_rows, list.STAT.f ) )

(
	st.m <-
		Reduce( bind_rows, list.STAT.m ) )

colSums( st.f, na.rm = T )
colSums( st.m, na.rm = T )

summary( st.f )
summary( st.m )

lms.f. <-
	melt( Reduce( bind_rows, list.LMS.f ), c( "age", "loop" ) ) 

lms.m. <-
	melt( Reduce( bind_rows, list.LMS.m ), c( "age", "loop" ) ) 

lms.f.$sex <-
	"female"

lms.m.$sex <-
	"male"

lms.f.$family <-
	"BCTo"

lms.m.$family <-
	"BCTo"

l <-
	as.data.frame( rbind( lms.f., lms.m. ) )

# l$loop <-
# 	as.factor( l$loop )

l. <-
	(
		l <-
		rename.columns(
			l,
			c( "sex", "family", "age", "variable", "value", "loop" ),
			c( "SEX", "FAMILY", "AGE", "VAR", "VAL", "LOOP" ) ) ) %>%
	group_by( SEX, FAMILY, AGE, VAR ) %>%
	summarise( MEAN.VAL = mean( VAL ) )

ggplot( ) +
		my.thm +
		geom_line( aes( AGE, VAL, group = paste0( LOOP, SEX ), col = SEX ), l, alpha = .1 ) +
		facet_grid( VAR ~ ., scales = "free" ) +
		scale_x_continuous( breaks = c( 0 : 20 ) ) +
		theme( axis.text.x = element_text( angle = 90 ) ) #+
		#geom_line( aes( AGE, MEAN.VAL, group = SEX ), l., col = "black" )

ggsubplot(
	ggplot( ) + 
		my.thm + 
		scale_x_continuous( breaks = c( 0 : 20 ) ) +
		geom_point( aes( AGE, CALCITONIN, col = SEX ), d, alpha = .1 ) +
		geom_smooth( aes( AGE, CALCITONIN, col = SEX ), d, method = "loess" ) +
		geom_line( aes( AGE, MEAN.VAL, group = SEX ), l.[ l.$VAR == "mu", ], col = "black" ),
	ggplot( ) +
		my.thm +
		geom_line( aes( AGE, VAL, group = paste0( LOOP, SEX ), col = SEX ), l, alpha = .1 ) +
		facet_grid( VAR ~ ., scales = "free" ) +
		scale_x_continuous( breaks = c( 0 : 20 ) ) +
		theme( axis.text.x = element_text( angle = 90 ) ) +
		geom_line( aes( AGE, MEAN.VAL, group = SEX ), l., col = "black" ),
	cols = 2 )

#save( l, list.LMS.f, list.LMS.m, list.STAT.f, list.STAT.m, file = "dat.Calcitonin.2017.09.12.Rd"  )

# load( file = "dat.Rd" )
# 
# l$LOOP <-
# 	as.numeric( l$LOOP )
# 
# l. <- 
# 	l
# 
# load( file = "dat1.Rd" )
# 
# l$LOOP <-
# 	as.numeric( l$LOOP )
# 
# l$LOOP <-
# 	l$LOOP + max( l.$LOOP )
# 
# l <-
# 	rbind( l., l )
# 
# l. <-
# 	(
# 		l <-
# 		rename.columns(
# 			l,
# 			c( "sex", "family", "age", "variable", "value", "loop" ),
# 			c( "SEX", "FAMILY", "AGE", "VAR", "VAL", "LOOP" ) ) ) %>%
# 	group_by( SEX, FAMILY, AGE, VAR ) %>%
# 	summarise( MEAN.VAL = mean( VAL, na.rm = T ) )


ggsubplot(
	ggplot( ) + 
		my.thm + 
		scale_x_continuous( breaks = c( 0 : 20 ) ) +
		geom_point( aes( AGE, CALCITONIN, col = SEX ), d, alpha = .1 ) +
		geom_smooth( aes( AGE, CALCITONIN, col = SEX ), d, method = "loess" ) +
		geom_line( aes( AGE, MEAN.VAL, group = SEX ), l.[ l.$VAR == "mu", ], col = "black" ),
	ggplot( ) +
		my.thm +
		geom_line( aes( AGE, VAL, group = paste0( LOOP, SEX ), col = SEX ), l, alpha = .01 ) +
		facet_grid( VAR ~ ., scales = "free" ) +
		scale_x_continuous( breaks = c( 0 : 20 ) ) +
		theme( axis.text.x = element_text( angle = 90 ) ) +
		geom_line( aes( AGE, MEAN.VAL, group = SEX ), l., col = "black" ),
	cols = 2 )

sds.BCTo <-
	function( perc, mu, sigma, nu, tau = 10 ) {

		z <-
			qnorm( perc )
				
		f <- 
			ifelse( 
				nu < .000001,
				mu * exp( sigma * z ),
				mu * ( z * nu * sigma + 1 ) ** ( 1 / nu ) ) 
		f }
	
sds.BCCGo <-
	function( perc, mu, sigma, nu ) {
		
		z <-
			qnorm( perc )
				
		f <- 
			ifelse( 
				nu < .000001,
				mu * exp( sigma * z ),
				mu * ( z * nu * sigma + 1 ) ** ( 1 / nu ) ) 
		f }

sds.NORM <-
	function( perc, mu, sigma ) qnorm( perc ) * sigma + mu


ll <-
	dcast( data = l., AGE + SEX + FAMILY ~ VAR, value.var = "MEAN.VAL" )

perc <-
	melt(
		perc.<-data.frame(
			age  = ll$AGE,
			sex  = ll$SEX, 
			p001 = sds.BCTo( .01, ll$mu, ll$sigma, ll$nu, ll$tau ),
			p003 = sds.BCTo( .03, ll$mu, ll$sigma, ll$nu, ll$tau ),
			p010 = sds.BCTo( .10, ll$mu, ll$sigma, ll$nu, ll$tau ),
			p025 = sds.BCTo( .25, ll$mu, ll$sigma, ll$nu, ll$tau ),
			p050 = sds.BCTo( .50, ll$mu, ll$sigma, ll$nu, ll$tau ),
			p075 = sds.BCTo( .75, ll$mu, ll$sigma, ll$nu, ll$tau ),
			p090 = sds.BCTo( .90, ll$mu, ll$sigma, ll$nu, ll$tau ),
			p097 = sds.BCTo( .97, ll$mu, ll$sigma, ll$nu, ll$tau ),
			p099 = sds.BCTo( .99, ll$mu, ll$sigma, ll$nu, ll$tau ) ),
		c( "sex", "age" ) )

ggplot( ) + 
	theme_bw( ) +
	geom_point( aes( AGE, CALCITONIN ), d, alpha = .1 ) +
	geom_line( aes( age, value, col = variable ), perc ) + facet_grid( sex ~ . )
	

perc.$age. <-
	cut( perc.$age, breaks = c(0:21))

perc.. <-
	perc. %>% group_by( age., sex ) %>% summarise( m = mean( p050 ) )

View( perc.. )

