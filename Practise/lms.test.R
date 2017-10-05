#!/usr/bin/env Rscript
rm( list = ls( ) )

arg <-
	commandArgs( trailingOnly = T )

loops <- 
	10

if( 0 < length( arg ) ) {
	
	loops <-
		as.numeric( arg ) }


# ifnot <-
# 	function( cond, optTrue, optFalse = { } ) {
# 		
# 		if( !cond ) {
# 			
# 			optTrue 
# 			
# 		} else {
# 			
# 			optFalse } }
# 
# list.append <-
# 	function( lst, x ) {
# 	
# 	lst[[ length( lst ) + 1 ]] <-
# 		x
# 	
# 	lst }

if( !"devtools" %in% rownames( installed.packages( ) ) ) {
	install.packages( "devtools" ) }

devtools::install_github( "TPeschel/hlpr4life" )

hlpr4life::load.pkgs(
	c(
		"hlpr4life",
		"ggplot2",
		"reshape2",
		"dplyr",
		"gamlss",
		"readxl" ) )

source( "get.mu.nu.sigma.tau.R" )

# d <-
# 	read_excel( "~/LIFE/life-for-postgraduates/JulianeWilz/data/AktuelleTabelle220517excel.xlsx" )

# d <-
# 	read_excel( "/home/tpeschel/LIFE/life-for-postgraduates/JulianeWilz/sent/data.2017.08.31/AktuelleTabelle190517excel.xlsx" )

# nt <-
# 	read_excel( "~/LIFE/life-for-postgraduates/JulianeWilz/data/NeueTabelle170517excel.xlsx" )
# 
nf <-
	read_excel( "~/LIFE/life-for-postgraduates/JulianeWilz/data/Nach_Filterung230417.xlsx" )

d <-
	nf

my.thm <-
	list(
		theme_bw( ),
		scale_color_manual( guide = F, values = c( "deeppink4", "deepskyblue4" ) ),
		scale_fill_manual( guide = F, values = c( "orange", "royalblue" ) ),
		facet_grid( SEX ~ . ),
		theme( axis.text.x = element_text( angle = 90, size = 7 ) ) )

my.thm.1 <-
	list(
		theme_bw( ),
		scale_color_brewer( type = "qual", palette = 2 ),
		scale_fill_brewer( type = "qual", palette = 2 ),
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
	geom_smooth( col = "black" ) +
	scale_x_discrete( limits = levels( d$AGE.BINS ) )

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

for( xtrans in c( F, T ) ) {
	
	for( loop in c( 1 : loops ) ) {
		
		print( paste0( "Loop: ", loop ) )
		
		print( "fit males" )
		
		MSNT.m <-
			get.lss( 
				d.m,
				ages = ages,
				family = c( "BE", "logNO", "NOF", "SEP1", "SEP2", "SEP3", "SEP4", "BCCGo", "BCPEo", "BCTo" ),
				sample.density = 1,
				trans.x = xtrans )
		
		if( !is.null( MSNT.m ) ) {
			
			MSNT.m$LMS$loop <-
				rep( length( list.LMS.m ) + 1, length( ages ) )
			
			list.LMS.m <-
				list.append( list = list.LMS.m, x = MSNT.m$LMS )
			
			list.STAT.m <-
				list.append( list = list.STAT.m, x = MSNT.m$STAT ) 
			
			print( paste0( "males fit success: ", length( list.LMS.m ) ) ) }
		
		print( "fit females" )
		
		MSNT.f <-
			get.lss( 
				d.f,
				ages = ages,
				family = c( "BE", "logNO", "NOF", "SEP1", "SEP2", "SEP3", "SEP4", "BCCGo", "BCPEo", "BCTo" ),
				sample.density = 1,
				trans.x = xtrans )
		
		if( !is.null( MSNT.f ) ) {
			
			MSNT.f$PRED$loop <-
				rep( length( list.LMS.f ) + 1, length( ages ) )
			
			list.LMS.f <-
				list.append( list = list.LMS.f, x = MSNT.f$LMS )
			
			list.STAT.f <-
				list.append( list = list.STAT.f, x = MSNT.f$STAT ) 
			
			print( paste0( "females fit success: ", length( list.LMS.f ) ) ) } } }

( 
	st.f <-
		Reduce( bind_rows, list.STAT.f ) )

(
	st.m <-
		Reduce( bind_rows, list.STAT.m ) )

( 
	cs.f <- 
		colSums( st.f, na.rm = T ) )

( 
	cs.m <- 
		colSums( st.m, na.rm = T ) )

(
	cs.f. <-
		cs.f[ names( cs.f ) != "Sum" ] )

(
	cs.m. <-
		cs.m[ names( cs.m ) != "Sum" ] )

( 
	bins <-
		union( as.numeric( names( cs.m. ) ), as.numeric( names( cs.f. ) ) ) )

cs. <-
	data.frame(
		bins = bins,
		f    = cs.f[ match( bins, as.numeric( names( cs.f ) ) ) ],
		m    = cs.m[ match( bins, as.numeric( names( cs.m ) ) ) ] )

cs.$f[ is.na( cs.$f ) ] <- 
	0

cs.$m[ is.na( cs.$m ) ] <- 
	0

cs. <-
	arrange( cs., as.numeric( as.character( cs.$bins ) ) )

cs.$f.x <-
	cumsum( cs.$f * cs.$bins )

cs.$m.x <-
	cumsum( cs.$m * cs.$bins )


ggplot( melt( cs.[ , c( "bins", "f.x", "m.x" ) ], "bins" ) ) +
	my.thm + facet_grid( . ~ variable ) +
	scale_color_manual( values = c( "deeppink", "deepskyblue" ) ) +
	geom_histogram( aes( bins, value, fill = variable ), stat = "identity", position = "dodge" )

ggplot( melt( cs., "bins" ) ) +
	theme_bw( ) +
	#	scale_fill_manual( values = c( "red", "blue" ), guide = F ) +
	#	geom_histogram( aes( bins, value, fill = variable ), stat = "identity" ) +
	geom_histogram( aes( log( bins ), value, fill = variable ), stat = "identity" ) +
	facet_grid( . ~ variable )

( 
	cs.m <- 
		colSums( st.m, na.rm = T ) )

ggplot( ) +
	theme_bw( ) +
	geom_histogram( aes( names( cs.f )))

summary( st.f )
summary( st.m )

lms.f. <-
	melt( Reduce( bind_rows, list.LMS.f ), c( "age", "loop", "family" ) ) 

lms.m. <-
	melt( Reduce( bind_rows, list.LMS.m ), c( "age", "loop", "family" ) ) 

lms.f.$sex <-
	"female"

lms.m.$sex <-
	"male"

# lms.f.$family <-
# 	"BCTo"
# 
# lms.m.$family <-
# 	"BCTo"

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
	my.thm.1 +
	scale_color_brewer( "SEX-FAMILY", type = "qual", palette = 2 ) +
	geom_line( aes( AGE, VAL, group = paste0( SEX, FAMILY, LOOP ), col = paste0( SEX, "-", FAMILY ) ), l, alpha = .1 ) +
	facet_grid( VAR ~ ., scales = "free" ) +
	scale_x_continuous( breaks = c( 0 : 20 ) ) +
	theme( axis.text.x = element_text( angle = 90 ) ) +
	geom_line( aes( AGE, MEAN.VAL, group = paste( SEX, FAMILY ), col = paste0( SEX, "-", FAMILY ) ), l. )

ggsubplot(
	ggplot( ) + 
		my.thm.1 + 
		facet_grid( SEX ~ ., scales = "free" ) +
		scale_color_brewer( "SEX-FAMILY", type = "qual", palette = 2, guide = F ) +
		scale_x_continuous( breaks = c( 0 : 20 ) ) +
		geom_point( aes( AGE, CALCITONIN ), d, col = "black", alpha = .1 ) +
		geom_smooth( aes( AGE, CALCITONIN ), d, col = "black", method = "loess" ) +
		geom_line( aes( AGE, MEAN.VAL, group = paste0( SEX, "-", FAMILY ), col = paste0( SEX, "-", FAMILY ) ), l.[ l.$VAR == "mu", ] ),
	ggplot( ) +
		my.thm.1 +
		scale_color_brewer( "SEX-FAMILY", type = "qual", palette = 2 ) +
		geom_line( aes( AGE, VAL, group = paste0( SEX, FAMILY, LOOP ), col = paste0( SEX, "-", FAMILY ) ), l, alpha = .1 ) +
		facet_grid( VAR ~ ., scales = "free" ) +
		scale_x_continuous( breaks = c( 0 : 20 ) ) +
		theme( axis.text.x = element_text( angle = 90 ) ) +
		geom_line( aes( AGE, MEAN.VAL, group = paste( SEX, FAMILY ), col = paste0( SEX, "-", FAMILY ) ), l. ),
	cols = 2 )


save( l, list.LMS.f, list.LMS.m, list.STAT.f, list.STAT.m, file = paste0( "LMS.Calcitonin.", gsub( "[-: CEST]","" ,Sys.time( ) ), ".Rd" ) )

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

ll <-
	dcast( data = l., AGE + SEX + FAMILY ~ VAR, value.var = "MEAN.VAL" )

ll.BCTo <-
	ll[ ll$FAMILY=="BCTo", ]

ll.BCPEo <-
	ll[ ll$FAMILY=="BCPEo", ]

ll.BCCGo <-
	ll[ ll$FAMILY=="BCCGo", ]

perc.BCTo <-
	data.frame(
		age  = ll.BCTo$AGE,
		sex  = ll.BCTo$SEX, 
		fam  = rep( "BCTo", length( ll.BCTo$AGE ) ),
		p001 = sds.BC( .01, ll.BCTo$mu, ll.BCTo$sigma, ll.BCTo$nu, ll.BCTo$tau ),
		p003 = sds.BC( .03, ll.BCTo$mu, ll.BCTo$sigma, ll.BCTo$nu, ll.BCTo$tau ),
		p010 = sds.BC( .10, ll.BCTo$mu, ll.BCTo$sigma, ll.BCTo$nu, ll.BCTo$tau ),
		p025 = sds.BC( .25, ll.BCTo$mu, ll.BCTo$sigma, ll.BCTo$nu, ll.BCTo$tau ),
		p050 = sds.BC( .50, ll.BCTo$mu, ll.BCTo$sigma, ll.BCTo$nu, ll.BCTo$tau ),
		p075 = sds.BC( .75, ll.BCTo$mu, ll.BCTo$sigma, ll.BCTo$nu, ll.BCTo$tau ),
		p090 = sds.BC( .90, ll.BCTo$mu, ll.BCTo$sigma, ll.BCTo$nu, ll.BCTo$tau ),
		p097 = sds.BC( .97, ll.BCTo$mu, ll.BCTo$sigma, ll.BCTo$nu, ll.BCTo$tau ),
		p099 = sds.BC( .99, ll.BCTo$mu, ll.BCTo$sigma, ll.BCTo$nu, ll.BCTo$tau ) )

perc.BCPEo <-
	data.frame(
		age  = ll.BCPEo$AGE,
		sex  = ll.BCPEo$SEX, 
		fam  = rep( "BCPEo", length( ll.BCPEo$AGE ) ),
		p001 = sds.BC( .01, ll.BCPEo$mu, ll.BCPEo$sigma, ll.BCPEo$nu, ll.BCPEo$tau ),
		p003 = sds.BC( .03, ll.BCPEo$mu, ll.BCPEo$sigma, ll.BCPEo$nu, ll.BCPEo$tau ),
		p010 = sds.BC( .10, ll.BCPEo$mu, ll.BCPEo$sigma, ll.BCPEo$nu, ll.BCPEo$tau ),
		p025 = sds.BC( .25, ll.BCPEo$mu, ll.BCPEo$sigma, ll.BCPEo$nu, ll.BCPEo$tau ),
		p050 = sds.BC( .50, ll.BCPEo$mu, ll.BCPEo$sigma, ll.BCPEo$nu, ll.BCPEo$tau ),
		p075 = sds.BC( .75, ll.BCPEo$mu, ll.BCPEo$sigma, ll.BCPEo$nu, ll.BCPEo$tau ),
		p090 = sds.BC( .90, ll.BCPEo$mu, ll.BCPEo$sigma, ll.BCPEo$nu, ll.BCPEo$tau ),
		p097 = sds.BC( .97, ll.BCPEo$mu, ll.BCPEo$sigma, ll.BCPEo$nu, ll.BCPEo$tau ),
		p099 = sds.BC( .99, ll.BCPEo$mu, ll.BCPEo$sigma, ll.BCPEo$nu, ll.BCPEo$tau ) )

perc.BCCGo <-
	data.frame(
		age  = ll.BCCGo$AGE,
		sex  = ll.BCCGo$SEX,
		fam  = rep( "BCCGo", length( ll.BCCGo$AGE ) ),
		p001 = sds.BC( .01, ll.BCCGo$mu, ll.BCCGo$sigma, ll.BCCGo$nu, ll.BCCGo$tau ),
		p003 = sds.BC( .03, ll.BCCGo$mu, ll.BCCGo$sigma, ll.BCCGo$nu, ll.BCCGo$tau ),
		p010 = sds.BC( .10, ll.BCCGo$mu, ll.BCCGo$sigma, ll.BCCGo$nu, ll.BCCGo$tau ),
		p025 = sds.BC( .25, ll.BCCGo$mu, ll.BCCGo$sigma, ll.BCCGo$nu, ll.BCCGo$tau ),
		p050 = sds.BC( .50, ll.BCCGo$mu, ll.BCCGo$sigma, ll.BCCGo$nu, ll.BCCGo$tau ),
		p075 = sds.BC( .75, ll.BCCGo$mu, ll.BCCGo$sigma, ll.BCCGo$nu, ll.BCCGo$tau ),
		p090 = sds.BC( .90, ll.BCCGo$mu, ll.BCCGo$sigma, ll.BCCGo$nu, ll.BCCGo$tau ),
		p097 = sds.BC( .97, ll.BCCGo$mu, ll.BCCGo$sigma, ll.BCCGo$nu, ll.BCCGo$tau ),
		p099 = sds.BC( .99, ll.BCCGo$mu, ll.BCCGo$sigma, ll.BCCGo$nu, ll.BCCGo$tau ) )

perc <-
	rbind(
		melt(
			perc.BCCGo,
			c( "sex", "age", "fam" ) ),
		melt(
			perc.BCPEo,
			c( "sex", "age", "fam" ) ),
		melt(
			perc.BCTo,
			c( "sex", "age", "fam" ) ) )

ggplot( ) + 
	theme_bw( ) +
	geom_point( aes( AGE, CALCITONIN ), d, alpha = .1 ) +
	geom_line( aes( age, value, group = variable, col = variable ), perc ) + facet_grid( sex ~ fam )

i<-0
l <- list( )

i<-i+10
l <- list.append( l, list( STAT = list( x = i+1:3 ), LMS = list( x = i+4:7 ) ) )

l
s <- Reduce( rbind, l )

str( s )

drop( Reduce( rbind, s[ 1 : 4 ] ) )

