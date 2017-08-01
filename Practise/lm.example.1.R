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

sex  <-
	c( "female", "male" ),

hair <-
	c( "black", "blonde", "brown" ),


params <-
	data.frame( 
		sex  = c( "female", "male" ),
		hair = c( "black", "blonde", "brown" ),
		mue  = c( 2, 4 ),
		beta = c( -.1, -.3 ), 
		var  = c( 2, 5 ) )

sex.levels <-
	c( "female", "male" )
	
hair.levels <-
	c( "black", "blonde", "brown" )


d <-
	data.frame(
		age  = a<-runif( n, 3, 18 ),
		sex  = s<-sample( sex.levels, n, T ),
		hair = h<-sample( hair.levels, n, T ),
		val  = 
			intersepts[ match( s, sex.levels ) * match( h, hair.levels ) ] + 
			slopes[ match( s, sex.levels ) * match( h, hair.levels ) ] * a +
			rnorm( n, 0, variances[ match( s, sex.levels ) * match( h, hair.levels ) ] ) )


colors()

my.colors <-
	"darkblue";

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
		lm( val ~ age * sex * hair, d ) )


summary( d.lm )

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
