rm( list = ls( ) )

hlpr4life::load.pkgs(
	c(
		"hlpr4life",
		"ggplot2",
		"gamlss" ) )

h4l.lmss.no.sex <-
	function( 
		y, 
		age, 
		data, 
		perc = c( 1, 10, 50, 10, 1 ),
		fams = c( "BCCGo", "BCPEo", "BCTo" ), 
		age.bins = seq( 0, 100, 1 / 12 ) ) {

		if( !y %in% names( data ) ) {
			
			print(
				paste0( 
					"There is no column named",
					y, ". Will return NA." ) )
			return( NA ) }
		
		if( !age %in% names( data ) ) {
			
			print(
				paste0( 
					"There is no column named",
					age, ". Will return NA." ) )
			return( NA ) }
		
		lms. <-
			gamlss::lms(
				y = data[ , y ],
				x = data[ , age ],
				families = fams )

		res. <-
			data.frame(
				RES = lms.$residuals,
				AGE = data[ , age ],
				MUE = lms.$y - lms.$residuals )

		lms. <-
			hlpr4life::rename.columns(
				as.data.frame( predict( lms., newdata = age.bins ) ),
				c( "mu", "sigma", "nu", "tau" ),
				c( "MUE", "SIGMA", "NUE", "TAU" ) )

		lms.$AGE <-
			age.bins

		list(
			MSNT = lms.,
			RESI = res. )
	}

h4l.lmss <-
	function( 
		y, 
		age, 
		sex, 
		data, 
		sex.lvls = c( "female", "male" ), 
		perc = c( 1, 10, 50, 10, 1 ),
		fams = c( "BCCGo", "BCPEo", "BCTo" ), 
		age.bins = seq( 0, round( max( data$age ) + 1 / 12, 1 / 12 ) ) ) {

		if( !y %in% names( data ) || !age %in% names( data ) || !sex %in% names( data ) ) {
			
			print(
				paste0( 
					"Can't neither find ",
					y, " nor ", age, " nor ", 
					sex, " as column name in data. Will return NA." ) )
			return( NA ) }

		d.sex.1 <-
			data[ data[ , sex ] == sex.lvls[ 1 ], ]
		
		d.sex.2 <-
			data[ data[ , sex ] == sex.lvls[ 2 ], ]
		
		lmss.1 <-
			h4l.lmss.no.sex( y, age, d.sex.1, perc, fams, age.bins )

		lmss.2 <-
			h4l.lmss.no.sex( y, age, d.sex.2, perc, fams, age.bins )
		
		lmss.1$MSNT$SEX <-
			sex.lvls[ 1 ]

		lmss.2$MSNT$SEX <-
			sex.lvls[ 2 ]
		
		lmss.1$RESI$SEX <-
			sex.lvls[ 1 ]
		
		lmss.2$RESI$SEX <-
			sex.lvls[ 2 ]
		
		list(
			MSNT = rbind( lmss.1$MSNT, lmss.2$MSNT ),
			RESI = rbind( lmss.1$RESI, lmss.2$RESI ) )
	}

sex.lvls <-
	c( "female", "male" )

sex.cols <-
	c( "deeppink", "deepskyblue" )

theme.sex <- 
	list(
		theme_bw( ),
		scale_color_manual( values = sex.cols, guide = F ),
		scale_fill_manual( values = sex.cols, guide = F ),
		facet_grid( . ~ SEX ) )

N <-
	1000

d <-
	data.frame(
		AGE = a<-runif( N, 0, 20 ),
		SEX = sample( sex.lvls, N, T ) )

d$Y <-
	c( 1, .8 )[ match( d$SEX, sex.lvls ) ] +
	c( 3, 4 )[ match( d$SEX, sex.lvls ) ] * exp( -d$AGE * c( 3, 4 )[ match( d$SEX, sex.lvls ) ] ) 
	

d$Y <-
	d$Y + rlnorm( N, 0, .1 * ( 20 - d$AGE ) )

d$logY <-
	log( d$Y ) - min( log( d$Y ) )

ggplot( d[ d$Y < 50, ], aes( AGE, Y, col = SEX ) ) + theme.sex + geom_point( ) + geom_smooth( )
ggplot( d, aes( AGE, logY, col = SEX ) ) + theme.sex + geom_point( ) + geom_smooth( )

ages <-
	seq( 0, 20, by = 1 / 12 )

lms.Y <-
	h4l.lmss.no.sex( y = "Y", age = "AGE", data = d[ d$SEX == "male", ], age.bins = ages )

lms.Y$RESI$

# lms.logY <-
# 	h4l.lmss( logY, age, sex, sex.lvls, data = d, age.bins = ages )
# 
# str( lms.Y )
# str( lms.logY )
# 
# ggsubplot(
# 	ggplot( ) + theme.sex + 
# 		geom_point( aes( AGE, MUE, col = SEX ), lms.Y$MSNT, alpha = .1 ),
# 	ggplot( ) + theme.sex +
# 		geom_point( aes( AGE, SIGMA, col = SEX ), lms.Y$MSNT, alpha = .1 ),
# 	ggplot( ) + theme.sex +
# 		geom_point( aes( AGE, NUE, col = SEX ), lms.Y$MSNT, alpha = .1 ),
# 	ggplot( ) + theme.sex +
# 		geom_point( aes( AGE, TAU, col = SEX ), lms.Y$MSNT, alpha = .1 ),
# 	ggplot( ) + theme.sex +
# 		geom_point( aes( age, y, col = sex ), d, alpha = .1 ) +
# 		geom_smooth( aes( age, y, col = sex ), d, alpha = .1 ),
# 	ggplot( ) + theme.sex +
# 		geom_point( aes( AGE, RES, col = SEX ), lms.Y$RESI, alpha = .1 ) +
# 		geom_smooth( aes( AGE, RES, col = SEX ), lms.Y$RESI, alpha = .1 ),
# 	ggplot( ) + theme.sex +
# 		geom_point( aes( AGE, MUE, col = SEX ), lms.Y$RESI, alpha = .1 ),
# 	ggplot( ) + theme.sex + 
# 		geom_point( aes( AGE, MUE, col = SEX ), lms.logY$MSNT, alpha = .1 ),
# 	ggplot( ) + theme.sex +
# 		geom_point( aes( AGE, SIGMA, col = SEX ), lms.logY$MSNT, alpha = .1 ),
# 	ggplot( ) + theme.sex +
# 		geom_point( aes( AGE, NUE, col = SEX ), lms.logY$MSNT, alpha = .1 ),
# 	ggplot( ) + theme.sex +
# 		geom_point( aes( AGE, TAU, col = SEX ), lms.logY$MSNT, alpha = .1 ),
# 	ggplot( ) + theme.sex +
# 		geom_point( aes( age, logY, col = sex ), d, alpha = .1 ) +
# 		geom_smooth( aes( age, logY, col = sex ), d, alpha = .1 ),
# 	ggplot( ) + theme.sex +
# 		geom_point( aes( AGE, RES, col = SEX ), lms.logY$RESI, alpha = .1 ) +
# 		geom_smooth( aes( AGE, RES, col = SEX ), lms.logY$RESI, alpha = .1 ),
# 	ggplot( ) + theme.sex +
# 		geom_point( aes( AGE, MUE, col = SEX ), lms.logY$RESI, alpha = .1 ),
# 	layout = t(
# 		matrix(
# 			c(
# 				1, 2, 5, 7,  8,  9, 12, 14,
# 				3, 4, 5, 7, 10, 11, 12, 14,
# 				6, 6, 6, 6, 13, 13, 13, 13 ),
# 			nrow = 8 ) ) )
# 
# 
# 
# ##########################################################################################################
N <-
	1000

d <-
	data.frame(
		AGE = runif( N, 0, 20 ),
		SEX = sample( sex.lvls, N, T ) )

d$Y <-
	c( -1, 1 )[ match( d$SEX, sex.lvls ) ] +
	c( +1 / 10, -1 / 10 )[ match( d$SEX, sex.lvls ) ] * d$AGE +
	rnorm( N, 0, 1 )

d$log10Y <-
	log10( d$Y - min( d$Y ) + 1 )

lmss.Y <-
	h4l.lmss.no.sex( y = "Y", age = "AGE", data = d )

l$r <-
	lms( Y, AGE, data = d )

lmss.logY <-
	h4l.lmss( y = log10Y, age = AGE, sex = SEX, data = d )

ggsubplot(
	ggplot( ) + theme.sex +
		geom_point( aes( age, y, col = sex ), d ) +
		geom_smooth( aes( age, y, col = sex ), d ),
	ggplot( ) + theme.sex +
		geom_point( aes( age, log10Y, col = sex ), d ) +
		geom_smooth( aes( age, log10Y, col = sex ), d ),
	cols = 2 )

ggsubplot(
	ggplot( ) + theme.sex +
		geom_point( aes( AGE, MUE, col = SEX ), lmss.Y$RESI ),
	ggplot( ) + theme.sex +
		geom_point( aes( AGE, RES, col = SEX ), lmss.Y$RESI ),
	ggplot( ) + theme.sex +
		geom_point( aes( AGE, MUE + RES, col = SEX ), lmss.Y$RESI ),
	ggplot( ) + theme.sex +
		geom_point( aes( age, y, col = sex ), d ),
	ggplot( ) + theme.sex +
		geom_point( aes( AGE, MUE, col = SEX ), lmss.logY$RESI ),
	ggplot( ) + theme.sex +
		geom_point( aes( AGE, RES, col = SEX ), lmss.logY$RESI ),
	ggplot( ) + theme.sex +
		geom_point( aes( AGE, MUE + RES, col = SEX ), lmss.logY$RESI ),
	ggplot( ) + theme.sex +
		geom_point( aes( age, log10Y, col = sex ), d ),
	cols = 4 )




