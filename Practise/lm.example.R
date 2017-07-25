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
	1000

intersept.male <-
	2

intersept.female <-
	4

intersepts <-
	c( intersept.female, intersept.male )

slope.male <-
	-.2

slope.female <-
	-.4

slopes <-
	c( slope.female, slope.male )

d <-
	data.frame(
		age = a<-runif( n, 3, 18 ),
		sex = s<-sample( sx<-c( "female", "male" ), n, T ),
		val = intersepts[ match( s, sx ) ] + slopes[ match( s, sx ) ] * a )

d$val[ d$sex == "male" ] <-
	d$val[ d$sex == "male" ] +
	rnorm( sum( d$sex == "male" ), 0, 2 )

d$val[ d$sex == "female" ] <- 
	d$val[ d$sex == "female" ] +
	rnorm( sum( d$sex == "female" ), 0, 3 )

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
	d.lm <-
		lm( val ~ age * sex, d ) )

ablines( d.lm )

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

d.lm
( ablines( d.lm ) )
