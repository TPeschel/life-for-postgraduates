if( !"devtools" %in% rownames( installed.packages( ) ) ) install.packages( "devtools" )

devtools::install_github( "TPeschel/hlpr4life" )

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
<<<<<<< HEAD

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
	+20

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
=======
>>>>>>> e54f79ced47f2649fffedd1c4cc354363c80c1eb

params <-
	data.frame( 
		sex  = c( "female", "male" ),
		mue  = c( 2, 4 ),
		beta = c( -.1, -.3 ), 
		var  = c( 2, 5 ) )

d <-
	data.frame(
		age  = a<-runif( n, 3, 18 ),
		sex  = s<-sample( params$sex, n, T ),
		val  = params$beta[ match( s, params$sex ) ] * a +
			rnorm( n, params$mue[ match( s, params$sex ) ], params$var[ match( s, params$sex ) ] ) )

f <-
	as.formula( val ~ age * hair * sex )

(
	d.lm <-
<<<<<<< HEAD
		lm( f, d ) )

d.lm

d.lm$terms

a<-attr( d.lm$terms, "factors" )

b<-d.lm$coefficients

a
b

colnames(attr(d.lm$terms, "factors"))
rownames(attr(d.lm$terms, "factors"))


summary( d.lm )
lm( val ~ age * sex, d )


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

