library( hlpr4life )
load.pkgs( c( "ggplot2" ) )

ablines <-
	function( model ) {
		
		coef <-
			model$coefficients
		
		contr <-
			model$contrasts
		
		if( contr == "contr.treatment" )
			contr <- c( 0, 1 )
		
		data.frame(
			intercept = c(
				coef[ 1 ] + contr[ 1 ] * coef[ 3 ],
				coef[ 1 ] + contr[ 2 ] * coef[ 3 ] ),
			
			slope = c(
				coef[ 2 ] + contr[ 1 ] * coef[ 4 ],
				coef[ 2 ] + contr[ 2 ] * coef[ 4 ] ),
			
			sex = c(
				"female",
				"male" ) ) }

n <- 
	100000

intersept.male.black <-
	1

intersept.male.blond <-
	2

intersept.male.brown <-
	3

intersept.female.black <-
	+11

intersept.female.blond <-
	-11

intersept.female.brown <-
	+20s

intersepts <-
	c( 
		intersept.female.black, 
		intersept.female.blond, 
		intersept.female.brown,
		intersept.male.black, 
		intersept.male.blond, 
		intersept.male.brown )

slope.female.black <-
	+.1

slope.female.blonde <-
	+.2

slope.female.brown <-
	+.3

slope.male.black <-
	-.1

slope.male.blonde <-
	-.2

slope.male.brown <-
	-.3

slopes <-
	c( 
		slope.female.black, 
		slope.female.blonde, 
		slope.female.brown, 
		slope.male.black, 
		slope.male.blonde, 
		slope.male.brown )

d <-
	data.frame(
		age = a<-runif( n, 3, 18 ),
		sex = s<-sample( sx<-c( "female", "male" ), n, T ),
		hair = h<-sample( hr<-as.factor( c( "blond", "brown", "black" ) ), n, T ),
		val = intersepts[ match( s, sx ) * match( h, hr )  ] + slopes[ match( s, sx ) * match( h, hr ) ] * a )

d$val[ d$sex == "male" ] <-
	d$val[ d$sex == "male" ] +
	rnorm( sum( d$sex == "male" ), 0, 2 )

d$val[ d$sex == "female" ] <- 
	d$val[ d$sex == "female" ] +
	rnorm( sum( d$sex == "female" ), 0, 11 )

(
	d.lm <-
		lm( val ~ age * sex * hair, d ) )

ablines( d.lm )



(
	p1 <-
		ggplot( ) +
		theme_bw( ) +
		geom_point( aes( age, val ), d ) )

(
	p2 <-
		ggplot( ) +
		theme_bw( ) +
		scale_color_manual( values = c( "deeppink", "deepskyblue" ), guide = F ) +
		geom_point( aes( age, val, col = sex ), d ) )

(
	p3 <-
		ggplot( ) +
		theme_bw( ) +
		scale_color_manual( values = c( "deeppink", "deepskyblue" ), guide = F ) +
		geom_point( aes( age, val, col = sex ), d ) +
		geom_abline( aes( intercept = intercept, slope = slope, group = sex, col = sex ), ablines( d.lm ) ) )

(
	p4 <-
		ggplot( ) +
		theme_bw( ) +
		scale_color_manual( values = c( "deeppink", "deepskyblue" ) ) +
		geom_point( aes( age, val, col = sex ), d ) +
		geom_abline( aes( intercept = intercept, slope = slope, group = sex, col = sex ), ablines( d.lm ) ) +
		facet_grid( sex ~ . ) )

ggsubplot(
	p1, p2, p3, p4,
	layout = t(
		matrix(
			c(
				1, 1, 1, 4, 4,
				2, 2, 2, 4, 4,
				3, 3, 3, 4, 4 ),
			ncol = 3 ) ) )
