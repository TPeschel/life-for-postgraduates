#######################################
# install devtools if not happend yet #
#######################################
if( !"devtools" %in% rownames( installed.packages( ) ) )
	install.packages( "devtools" )
devtools::install_github( "TPeschel/hlpr4life" )

################
# clear memory #
################
rm( list = ls( ) )

####################################
# install and load needed packages #
####################################
hlpr4life::load.pkgs(
	c(
		"hlpr4life",
		"dplyr",
		"reshape",
		"ggplot2",
		"ggthemes",
		"MASS",
		"lsr" ) )

#################################
# define some colors for ggplot #
#################################
sex.colors <-
	c( "deeppink", "deepskyblue" )

smoke.colors <-
	c( "green", "red" )

sex.smoke.colors <-
	c( "deeppink1", "deeppink4", "deepskyblue", "deepskyblue4" )

#################################################
# load data fevDat from harddisk if it is there #
# if not then read it from the inet source      #
#################################################
if( "fevDat.Rd" %in% dir( ) ) { 
	load( "fevDat.Rd" ) } else {
	fevDat <-                   
		read.table(             
			"http://www.emersonstatistics.com/Datasets/fev.txt",
			header = TRUE )
	save( fevDat, file = "fevDat.Rd" ) }

###############################
# call it "a" for convenience #
###############################
a <-
	fevDat

##################
# show structure #
##################
str( a )

####################################
# sex and smoke have to be recoded #
####################################
a$sex <-
	as.factor( c( "male", "female" )[ match( a$sex, c( 1, 2 ) ) ] )

a$smoke <-
	as.factor( c( "smoker", "non-smoker" )[ match( a$smoke, c( 1, 2 ) ) ] )

##################
# show structure #
##################
str( a )

summary( fevDat[ is.numeric( fevDat ) ] )

ggplot( a ) +
	theme_bw( ) +
	scale_color_manual("groups",  values = sex.smoke.colors ) +
	scale_fill_manual( "groups", values = sex.smoke.colors ) +
	geom_histogram( aes( age, fill = paste0( sex, ":", smoke ) ), stat = "count" ) +
	facet_grid( sex ~ smoke  )

ggplot( a, aes( age, fev, col = paste0( sex, ":", smoke ) ) ) +
	theme_bw( ) +
	scale_color_manual("groups",  values = sex.smoke.colors ) +
	scale_fill_manual( "groups", values = sex.smoke.colors ) +
	geom_point( ) +
	geom_smooth( method = "lm", col = "black" ) +
	geom_smooth( ) +
	facet_grid( sex ~ smoke  )

ggplot( a, aes( age, height, col = paste0( sex, ":", smoke ) ) ) +
	theme_bw( ) +
	scale_color_manual("groups",  values = sex.smoke.colors ) +
	scale_fill_manual( "groups", values = sex.smoke.colors ) +
	geom_point( ) +
	geom_smooth( method = "lm", col = "black" ) +
	geom_smooth( col = "green" ) +
	facet_grid( sex ~ smoke  )

ggplot( a, aes( height**(3), fev, col = paste0( sex, ":", smoke ) ) ) +
	theme_bw( ) +
	scale_color_manual("groups",  values = sex.smoke.colors ) +
	scale_fill_manual( "groups", values = sex.smoke.colors ) +
	geom_point( ) +
	geom_smooth( method = "lm", col = "black" ) +
#	geom_smooth( ) +
	facet_grid( sex ~ smoke  )

a.smoking.age <-
	range( a$age[ a$smoke == "smoker" ] )

a.comparable <-
	a[ a.smoking.age[ 1 ] <= a$age & a$age <= a.smoking.age[ 2 ], ]

ggplot( a.comparable, aes( age, height, col = paste0( sex, ":", smoke ) ) ) +
	theme_bw( ) +
	scale_color_manual("groups",  values = sex.smoke.colors ) +
	scale_fill_manual( "groups", values = sex.smoke.colors ) +
	geom_point( ) +
	geom_smooth( method = "lm", col = "black" ) +
	geom_smooth( col = "green" ) +
	facet_grid( sex ~ smoke  )

a.comparable <-
	adjust.std( height ~ age * sex, a.comparable )

ggsubplot(
	ggplot( a.comparable, aes( age, height, col = paste0( sex, ":", smoke ) ) ) +
		theme_bw( ) +
		scale_color_manual("groups",  values = sex.smoke.colors, guide = F ) +
		scale_fill_manual( "groups", values = sex.smoke.colors, guide = F ) +
		geom_point( ) +
		geom_smooth( method = "lm", col = "black" ) +
		geom_smooth( col = "green" ) +
		facet_grid( sex ~ smoke  ),
	ggplot( a.comparable, aes( age, height.std.for.age.sex, col = paste0( sex, ":", smoke ) ) ) +
		theme_bw( ) +
		scale_color_manual("groups",  values = sex.smoke.colors, guide = F ) +
		scale_fill_manual( "groups", values = sex.smoke.colors, guide = F ) +
		geom_point( ) +
		geom_smooth( method = "lm", col = "black" ) +
		geom_smooth( col = "green" ) +
		facet_grid( sex ~ smoke  ),
	cols = 2 )

b <-
	a[ a$subjid %in% a.comparable$subjid, ]

b <-
	adjust.std( height ~ age, b )

ggsubplot(
	ggplot( b, aes( height, fev, col = paste0( sex, ":", smoke ) ) ) +
		theme_bw( ) +
		scale_color_manual("groups",  values = sex.smoke.colors, guide = F ) +
		scale_fill_manual( "groups", values = sex.smoke.colors, guide = F ) +
		geom_point( ) +
		geom_smooth( method = "lm", col = "black" ) +
		geom_smooth( col = "green" ) +
		facet_grid( sex ~ smoke  ),
	ggplot( b, aes( height.std.for.age, fev, col = paste0( sex, ":", smoke ) ) ) +
		theme_bw( ) +
		scale_color_manual("groups",  values = sex.smoke.colors, guide = F ) +
		scale_fill_manual( "groups", values = sex.smoke.colors, guide = F ) +
		geom_point( ) +
		geom_smooth( method = "lm", col = "black" ) +
		geom_smooth( col = "green" ) +
		facet_grid( sex ~ smoke  ),
	cols = 2 )

summary( lm( fev ~ height.adj.for.age.sex * sex * smoke, b ) )
summary( lm( fev ~ height.std.for.age * sex * smoke, b ) )

ggsubplot(
	ggplot( b, aes( height.std.for.age, fev, col = paste0( sex, ":", smoke ), group = paste0( sex, ":", smoke ) ) ) +
		theme_bw( ) +
		scale_color_manual("groups",  values = sex.smoke.colors, guide = F ) +
		scale_fill_manual( "groups", values = sex.smoke.colors, guide = F ) +
		geom_point( ) +
		geom_smooth( method = "lm" ),
	ggplot( b, aes( height, fev, col = paste0( sex, ":", smoke ), group = paste0( sex, ":", smoke ) ) ) +
		theme_bw( ) +
		scale_color_manual("groups",  values = sex.smoke.colors, guide = F ) +
		scale_fill_manual( "groups", values = sex.smoke.colors, guide = F ) +
		geom_point( ) +
		geom_smooth( method = "lm" ),
	cols = 2 )
