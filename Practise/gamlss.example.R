rm( list = ls( ) )

hlpr4life::load.pkgs(
	c(
		"hlpr4life",
		"ggplot2",
		"gamlss" ) )

h4l.lmss <-
	function( 
		y, 
		age, 
		sex, 
		sex.lvls = c( "female", "male" ), 
		perc = c( 1, 10, 50, 10, 1 ),
		fams = c( "BCCGo", "BCPEo", "BCTo" ), 
		data, 
		age.bins = seq( 0, round( max( data$age ) + 1 / 12, 1 / 12 ) ) ) {

		d.sex.1 <-
			data[ data$sex == sex.lvls[ 1 ], ]
		
		lms.1 <-
			lms(
				y = y,
				x = age,
				families = fams,
				data = d.sex.1 )
		
		lms.1 <-
			as.data.frame( predict( lms.1, newdata = age.bins ) )

		lms.1$sex <-
			sex.lvls[ 1 ]
		
		lms.1$age <-
			age.bins
		
		d.sex.2 <-
			data[ data$sex == sex.lvls[ 2 ], ]
		
		lms.2 <-
			lms(
				y = y,
				x = age,
				families = fams,
				data = d.sex.2 )
		
		lms.2 <-
			as.data.frame( predict( lms.2, newdata = age.bins ) )
		
		lms.2$sex <-
			sex.lvls[ 2 ]
		
		lms.2$age <-
			age.bins
		
		rbind( lms.1, lms.2 )
	}

sex.lvls <-
	c( "female", "male" )

sex.cols <-
	c( "deeppink", "deepskyblue" )

N <-
	1000

d <-
	data.frame(
		age = a<-runif( N, 0, 20 ),
		sex = sample( sex.lvls, N, T ) )

d$y <-
	c( 1, .8 )[ match( d$sex, sex.lvls ) ] +
	c( 3, 4 )[ match( d$sex, sex.lvls ) ] * exp( -d$age * c( 3, 4 )[ match( d$sex, sex.lvls ) ] ) 
	
d$y <-
	d$y + rlnorm( N, 0, .1 * ( 20 - d$age ) )

d$logY <-
	log( d$y )

ggplot( d, aes( age, y, col = sex ) ) + theme_bw( ) + scale_color_manual( values = sex.cols ) + geom_point( ) + geom_smooth( )
ggplot( d, aes( age, logY, col = sex ) ) + theme_bw( ) + scale_color_manual( values = sex.cols ) + geom_point( ) + geom_smooth( )

ages <-
	seq( 0, 20, by = 1 / 12 )

lms.Y <-
	h4l.lmss( y, age, sex, sex.lvls, data = d, age.bins = ages )

lms.logY <-
	h4l.lmss( logY, age, sex, sex.lvls, data = d, age.bins = ages )

str( lms.logY )

ggplot( ) +
	theme_bw( ) + scale_color_manual( values = sex.cols ) +
	geom_point( aes( age, logY, col = sex ), d, alpha = .1 ) +
	geom_point( aes( age, mu, col = sex ), lms.logY ) +
#	geom_line( aes( age, mu, col = sex ), lms.logY ) +
	geom_smooth( aes( age, logY, col = sex ), d, method = "loess" )

lms.f <-
	lms( y = logY, x = age, data = d[ d$sex == "female", ] )

f <-
	as.data.frame(
		predict(
			lms.f,
			newdata = ages ) )

f$sex <-
	"female"

f$age <-
	ages

lms.m <-
	lms( y = logY, x = age, data = d[ d$sex == "male", ] )

m <-
	as.data.frame(
		predict(
			lms.m,
			newdata = ages ) )

m$sex <-
	"male"

m$age <-
	ages

l <-
	rbind( f, m )

ggplot( ) + 
	geom_point( aes( age, mu, col = sex ), l ) +
	geom_point( aes( age, logY, col = sex ), d )

f <-
	as.data.frame(
		predict(
			lms( y = y, x = age, data = d[ d$sex == "female", ] ),
			newdata = ages ) )

f$sex <-
	"female"

f$age <-
	ages

m <-
	as.data.frame(
		predict(
			lms( y = y, x = age, data = d[ d$sex == "male", ] ),
			newdata = ages ) )

m$sex <-
	"male"

m$age <-
	ages

l <-
	rbind( f, m )

ggplot( ) + 
	geom_point( aes( age, mu, col = sex ), l ) +
	geom_point( aes( age, y, col = sex ), d ) + 
	geom_smooth( aes( age, y, col = sex ), d ) + 
	ylim( c( 0, 5 ) )

