rm( list = ls( ) )

devtools::install_github( "TPeschel/hlpr4life" )

library( hlpr4life )

load.pkgs( c( "ggplot2", "broom", "reshape2", "lme4" ) )

(
	sex.lvls <-
		c( "female", "male" ) )
	
(
	fams <-
		c( "Mueller", "Meier", "Schulz", "Schmidt", "Merkel", "Antropov", "Clinton", "Saint-Exupery" ) )
	
(
	hair.lvls <-
		c( "black", "blonde", "brown" ) )

( 
	prms <-
		data.frame(
			lvls = as.vector( sapply( sex.lvls, function( l ) paste0( l, ":", hair.lvls ) ) ),
			mue  = m<-runif( 6, -10, +10 ),
			var  = runif( 6,  +10, +20 ),
			slp  = -m / 5  ) )

( 
	cols.sex.hair <-
		c( "deeppink", "deeppink2", "deeppink4", "deepskyblue", "deepskyblue2", "deepskyblue4" ) )
( 
	cols.sex <-
		c( "deeppink", "deepskyblue" ) )
( 
	cols.hair <-
		c( "#302020", "#d0a030", "#a88020" ) )

# ( 
# 	cols.sex.hair <-
# 		c( "#202020", "#c0a030", "#a08020", "#302010", "#d09020", "#b08010" ) )
# ( 
# 	cols.sex <-
# 		c( "deeppink", "deepskyblue" ) )
# ( 
# 	cols.hair <-
# 		c( "#302020", "#d0a030", "#a88020" ) )

( 
	n <-
		1000 )

d <-
	data.frame(
		fam  = sample( fams, n, T ),
		age  = runif( n, 3, 18 ),
		sex  = sample( sex.lvls, n, T ),
		hair = sample( hair.lvls, n, T ) )
	
d$y <-
	rnorm( n, prms$mue[ match( paste0( d$sex, ":", d$hair ), prms$lvls ) ], prms$var[ match( paste0( d$sex, ":", d$hair ), prms$lvls ) ] ) +
	d$age * prms$slp[ match( paste0( d$sex, ":", d$hair ), prms$lvls ) ] +
	d$age * runif( length( fams ), -1, 1 )[ match( d$fam, fams, T ) ] 

(
	lm.d.y.age <-
		lm( y ~ age, d ) )

summary( lm.d.y.age )

ggplot( 
	d,
	aes( age, y ) ) +
	theme_bw( ) +
	geom_point( alpha = .1 ) +
	geom_smooth( method = "lm" )

(
	lm.d.y.age.sex <-
		lm( y ~ age * sex, d ) )

summary( lm.d.y.age.sex )

ggplot( 
	d,
	aes( age, y, col = sex ) ) +
	theme_bw( ) +
	geom_point( alpha = .1 ) +
	geom_smooth( method = "lm" ) +
	scale_color_manual( values = cols.sex, guide = F )

anova( lm.d.y.age, lm.d.y.age.sex )

(
	lm.d.y.age.hair <-
		lm( y ~ age * hair, d ) )

summary( lm.d.y.age.hair )

ggplot( 
	d,
	aes( age, y, col = hair ) ) +
	theme_bw( ) +
	geom_point( alpha = .1 ) +
	geom_smooth( method = "lm" ) +
	scale_color_manual( values = cols.hair, guide = F )

anova( lm.d.y.age, lm.d.y.age.hair )

anova( lm.d.y.age.sex, lm.d.y.age.hair )

(
	lm.d.y.age.sex.hair <-
		lm( y ~ age * sex * hair, d ) )

summary( lm.d.y.age.sex.hair )
anova( lm.d.y.age.sex, lm.d.y.age.hair )
anova( lm.d.y.age.sex.hair, lm.d.y.age.hair )
anova( lm.d.y.age.sex, lm.d.y.age.sex.hair )

ggplot( 
	d,
	aes( age, y, col = paste0( sex, ":", hair ) ) ) +
	theme_bw( ) +
	geom_point( alpha = .1 ) +
	geom_smooth( method = "lm" ) +
	scale_color_manual( values = cols.sex.hair, guide = F )

ggplot( 
	d,
	aes( age, y, col = paste0( sex, ":", hair ) ) ) +
	theme_bw( ) +
	geom_point( alpha = .1 ) +
	geom_smooth( method = "lm" ) +
	facet_grid( sex ~ hair ) +
	scale_color_manual( values = cols.sex.hair, guide = F )

ggplot( 
	d,
	aes( age, y, col = paste0( sex, ":", hair ) ) ) +
	theme_bw( ) +
	geom_point( alpha = .1 ) +
	geom_smooth( method = "lm" ) +
	facet_grid( . ~ sex ) +
	scale_color_manual( values = cols.sex.hair, guide = F )

ggplot( 
	d,
	aes( age, y, col = paste0( sex, ":", hair ) ) ) +
	theme_bw( ) +
	geom_point( alpha = .1 ) +
	geom_smooth( method = "lm" ) +
	facet_grid( . ~ hair ) +
	scale_color_manual( values = cols.sex.hair, guide = F )


lm.d.y.age.sex.hair$terms

m <-
	attr( lm.d.y.age.sex.hair$terms, "factors" )

l <-
	 lm.d.y.age.sex.hair$coefficients

age <-
	c( 3 : 18 )

( y0_female_black  <- l[ "(Intercept)" ] )

( y0_female_blonde <- l[ "(Intercept)" ] + l[ "hairblonde" ] )
( y0_female_brown  <- l[ "(Intercept)" ] + l[ "hairbrown" ] )
( y0_male_black    <- l[ "(Intercept)" ] + l[ "sexmale" ] )

( y0_male_blonde   <- l[ "(Intercept)" ] + l[ "sexmale" ] + l[ "hairblonde" ] + l[ "sexmale:hairblonde" ] )
( y0_male_brown    <- l[ "(Intercept)" ] + l[ "sexmale" ] + l[ "hairbrown" ]  + l[ "sexmale:hairbrown" ] )

( beta_female_black  <- l[ "age" ] )
( beta_female_blonde <- l[ "age" ] + l[ "age:hairblonde" ] )
( beta_female_brown  <- l[ "age" ] + l[ "age:hairbrown" ] )
( beta_male_black    <- l[ "age" ] + l[ "age:sexmale" ] )
( beta_male_blonde   <- l[ "age" ] + l[ "age:sexmale" ] + l[ "age:hairblonde" ] + l[ "age:sexmale:hairblonde" ] )
( beta_male_brown    <- l[ "age" ] + l[ "age:sexmale" ] + l[ "age:hairbrown" ]  + l[ "age:sexmale:hairbrown" ] )

(
	plt <-
		ggplot( 
			d,
			aes( age, y, col = paste0( sex, ":", hair ) ) ) +
			theme_bw( ) +
			geom_point( alpha = .1 ) +
			geom_smooth( method = "lm" ) +
			facet_grid( sex ~ hair ) +
			scale_color_manual( values = cols.sex.hair, guide = F ) )


(
	fit <-
		data.frame(
			sex = c(
				rep( sex.lvls[ 1 ], 3 ),
				rep( sex.lvls[ 2 ], 3 ) ),
			hair = rep( hair.lvls, 2 ),
			y0 = c(
				y0_female_black,
				y0_female_blonde,
				y0_female_brown,
				y0_male_black,
				y0_male_blonde,
				y0_male_brown ),
			bt = c(
				beta_female_black,
				beta_female_blonde,
				beta_female_brown,
				beta_male_black,
				beta_male_blonde,
				beta_male_brown ) ) )

fit$xs <-
	3

fit$ys <-
	fit$y0 + fit$bt * fit$xs

fit$xf <-
	18

fit$yf <-
	fit$y0 + fit$bt * fit$xf

(
	lmts <-
		data.frame(
			age           = a <- c( 3, 18 ),
			female_black  = y0_female_black  + beta_female_black * a,
			female_blonde = y0_female_blonde + beta_female_blonde * a,
			female_brown  = y0_female_brown  + beta_female_brown * a,
			male_black    = y0_male_black    + beta_male_black * a,
			male_blonde   = y0_male_blonde   + beta_male_blonde * a,
			male_brown    = y0_male_brown    + beta_male_brown * a ) )

plt + 
	geom_point( aes( xs, ys, col = paste0( fit$sex, ":", fit$hair ) ), fit, shape = 1, size = 3 ) +
	geom_point( aes( xf, yf, col = paste0( fit$sex, ":", fit$hair ) ), fit, shape = 2, size = 3 )


tidy( lm.d.y.age.sex.hair )
fit
prms


lmer.d.y.age <-
	lmer( y ~ age + ( 1 | fam ), d, REML = F )

lmer.d.y.age.sex <-
	lmer( y ~ age * sex + ( 1 | fam ), d, REML = F )

lmer.d.y.age.hair <-
	lmer( y ~ age * hair + ( 1 | fam ), d, REML = F )

lmer.d.y.age.sex.hair <-
	lmer( y ~ age * sex * hair + ( 1 | fam ), d, REML = F )

anova( lmer.d.y.age, lmer.d.y.age.hair )
anova( lmer.d.y.age, lmer.d.y.age.sex )
anova( lmer.d.y.age.hair, lmer.d.y.age.sex )
anova( lmer.d.y.age.sex.hair, lmer.d.y.age.sex )
