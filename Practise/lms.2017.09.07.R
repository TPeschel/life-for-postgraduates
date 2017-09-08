rm( list = ls( ) )

load.pkgs(
	c(
		"ggplot2",
		"ggthemes",
		"gamlss" ) )

lms.shrt <-
	function( data, families = c( "BCGGo", "BCPEo", "BCTo" ), perc = c( 3, 10, 25, 50, 75, 90, 97 ), x.bins = data$x ) {
		
		# data<-b
		# families<-c("BCTo")
		# perc<-c( 3, 10, 25, 50, 75, 90, 97 )
		# x.bins<-seq( 0, 20, .1 )

		lms. <-
			gamlss::lms(
				y = y,
				x = x,
				data = data,
				families = families,
				cent = perc )
		
		lms.$family
		lms.$failed

		res. <-
			gamlss::predictAll( 
				lms., 
				newdata = data.frame( x = x.bins ) )
		
		res.$x <-
			x.bins
		
		if( lms.$family[ 1 ] %in% c( "NO" ) ) {
			
			res. <-
				as.data.frame(
					hlpr4life::rename.list(
						res.,
						c( "x", "mu", "sigma" ),
						c( "X", "MUE", "SIG" ) ) )
				
				res. <-
					res.[ , c( "X", "MUE", "SIG" ) ]
				
			return(
				list(
					PRED = res.,
					FAIL = lms.$failed ) ) }
			
		if( lms.$family[ 1 ] %in% c( "BCCG", "BCPE", "BCCGo", "BCPEo" ) ) {
			
			res. <-
				as.data.frame(
					hlpr4life::rename.list(
						res.,
						c( "x", "mu", "sigma", "nu" ),
						c( "X", "MUE", "SIG", "NUE" ) ) )
			
			res. <-
				res.[ , c( "X", "MUE", "SIG", "NUE" ) ]

			return(
				list(
					PRED = res.,
					FAIL = lms.$failed ) ) }
		
		if( lms.$family[ 1 ] %in% c( "BCT", "BCTo" ) ) {
			
			res. <-
				as.data.frame(
					hlpr4life::rename.list(
						res.,
						c( "x", "mu", "sigma", "nu", "tau" ),
						c( "X", "MUE", "SIG", "NUE", "TAU" ) ) )
			
			res. <-
				res.[ , c( "X", "MUE", "SIG", "NUE", "TAU" ) ]
			
			return(
				list(
					PRED = res.,
					FAIL = lms.$failed ) ) }
		}

(a<-data.frame(x=x<-runif(1000,0,10),y=abs(x**2+rnorm(1000,10,2))))
(b<-hlpr4life::rename.columns(a,c("age","val"),c("x","y")))
(lms.<-lms.shrt(b,families = c( "NO", "BCCG", "BCPE", "BCT", "BCGGo", "BCPEo", "BCTo" ) ) )

ggsubplot(
	ggplot( ) + theme_bw( ) + scale_color_manual( values = c( "deeppink", "deepskyblue" ) ) +
		geom_point( aes( x, y ), b, col = "blue", alpha = .1 ) +
		geom_line( aes( X, MUE ), lms.$PRED, col = "red" ) +
		geom_line( aes( X, MUE - SIG ), lms.$PRED, col = "black" ) +
		geom_line( aes( X, MUE + SIG ), lms.$PRED, col = "green" ),
	
	ggplot( ) + theme_bw( ) + scale_color_manual( values = c( "deeppink", "deepskyblue" ) ) +
		geom_line( aes( X, exp( -( ( MUE ) ** 2 ) / ( 2 * SIG ) ** 2 ) ), lms.$PRED, col = "orange" ),
	
	cols = 1 )

plot(qnorm(p = c(.9),lms.$PRED$MUE,lms.$PRED$SIG))
quantile(pnorm( q = c(10,90) ,lms.$PRED$MUE,lms.$PRED$SIG),c(0.1,.9))
?qnorm

erf <- function(x) 2 * pnorm(x * sqrt(2)) - 1
x<-rnorm( 300 )
str(quantile( x, probs = c( .03, .1, .25, .5, .75, .9, .97 ) ))

q <-
	quantile( x, probs = c( .025, .05, .1, .25, .5, .75, .9, .95, .975 ) )

names( q ) <- attr( q, "names" )
ggplot( ) + theme_bw( ) + geom_line( aes( x, exp( -x**2 / 2 ) / sqrt( 2 * pi ) ) ) + geom_line( aes( x, .5 * ( 1 + erf( x ) ) ) ) + geom_vline( aes( xintercept = ( reshape2::melt( q ) )$value, col = rownames( ( reshape2::melt( q ) ) ) ) )
