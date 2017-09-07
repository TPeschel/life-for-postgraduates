##########################################################
rm( list = ls( ) )                                       #
if( !"devtools" %in% rownames( installed.packages( ) ) ) #
	install.packages( "devtools" )                       #
devtools::install_github( "TPeschel/hlpr4life" )         #
##########################################################
hlpr4life::load.pkgs( #
	c(                #
		"hlpr4life",  #
		"ggplot2",    #
		"reshape2",   #
		"lsr",        #
		"dplyr" ) )   #
#######################

Geschaeft <-
	data.frame( 
		Tag = rep( c( 1 : 5 ), 3 ),
		Ort = c(
			rep( "Normalregal", 5 ),
			rep( "Zweitplazierung", 5 ),
			rep( "Kühlregal", 5 ) ),
		Becher = c( 
			47, 39, 40, 46, 45,
			68, 65, 63, 59, 67,
			59, 50, 51, 48, 53 ),
		Papier = c( 
			40, 39, 35, 36, 37,
			59, 57, 54, 56, 53,
			53, 47, 48, 50, 51 ) )

Geschaeft.melt <-
	reshape2::melt( 
		Geschaeft,
		c(
			"Tag",
			"Ort" ) )

names( Geschaeft.melt )[ names( Geschaeft.melt ) == "variable" ] <-
	"Verpackung"

names( Geschaeft.melt )[ names( Geschaeft.melt ) == "value" ] <-
	"Anzahl"

Geschaeft
Geschaeft.melt

Geschaeft.melt$Ort <-
	relevel( Geschaeft.melt$Ort, "Normalregal" )

Geschaeft.melt$Verpackung <-
	relevel( Geschaeft.melt$Verpackung, "Papier" )

(
	summary.lm.Anazahl_Tag.mal.Ort.mal.Verpackung <-
		summary(
			lm.Anazahl_Tag.mal.Ort.mal.Verpackung <-
				lm( Anzahl ~ Tag * Ort * Verpackung, Geschaeft.melt ) ) )

(
	summary.lm.Anazahl_Tag.plus.Ort.plus.Verpackung <-
		summary(
			lm.Anazahl_Tag.plus.Ort.plus.Verpackung <-
				lm( Anzahl ~ Tag + Ort + Verpackung, Geschaeft.melt ) ) )

(
	summary.lm.Anazahl.Ort.mal.Verpackung <-
		summary(
			lm.Anazahl.Ort.mal.Verpackung <-
				lm( Anzahl ~ Ort * Verpackung, Geschaeft.melt ) ) )

(
	summary.lm.Anazahl.Ort.mal.Verpackung <-
		summary(
			lm.Anazahl.Ort.mal.Verpackung <-
				lm( Anzahl ~ Ort * Verpackung, Geschaeft.melt ) ) )

p0 <-
	ggplot(
		Geschaeft.melt, 
		aes(
			Tag,
			Anzahl,
			paste0( Ort, ":", Verpackung ) ) ) +
	geom_point( size = 3 ) + 
	geom_smooth(  alpha = .2, method = "lm", col = "black" ) +
	theme_bw( ) +
	scale_color_discrete(
		"Gruppen", guide = F )

p1 <-
	ggplot(
		Geschaeft.melt, 
		aes(
			Tag,
			Anzahl,
			col = paste0( Ort, ":", Verpackung ) ) ) +
	geom_point( size = 3 ) + 
	geom_smooth(  alpha = .2, method = "lm" ) +
	theme_bw( ) +
	scale_color_discrete(
		"Gruppen", guide = F )

Geschaeft.Gruppenmittelwerte <-
	Geschaeft.melt %>%
		group_by( Ort, Verpackung ) %>%
		summarise( Absatzmenge = mean( Anzahl ) )

p2 <-
	ggplot(
		Geschaeft.Gruppenmittelwerte,
		aes(
			Verpackung,
			Absatzmenge,
			col = Ort,
			group = Ort ) ) +
	theme_bw( ) +
	theme(
		legend.position = c( .15, .75 ) ) +
	geom_line( size = .68 ) +
	geom_point( size = 3 ) 

p3 <-
	ggplot(
		Geschaeft.Gruppenmittelwerte,
		aes(
			Ort,
			Absatzmenge,
			col = Verpackung,
			group = Verpackung ) ) +
	theme_bw( ) +
	theme(
		legend.position = c( .25, .75 ) ) +
	geom_line( size = .68 ) +
	geom_point( size = 3 ) 

ggsubplot(
	p0,
	p1,
	p1 + facet_grid( Ort ~ Verpackung ),
	p1 + facet_grid( Verpackung ~ Ort ),
	p2,
	p3,
	layout = t(
		matrix(
			c(
				1, 2,
				3, 4,
				5, 6 ),
			nrow = 2 ) ) )
	
summary( lm.Anazahl_Tag.mal.Ort.mal.Verpackung )

lsr::etaSquared( lm.Anazahl_Tag.mal.Ort.mal.Verpackung )

