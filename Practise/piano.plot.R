rm( list = ls( ) )

library( "hlpr4life" )

load.pkgs( c( "dplyr", "ggplot2", "ggthemes" ) )

the.piano <-
	piano( 4, 88 )

study <-
	merge( 
		the.piano,
		data.frame( 
			sex  = s<-sample( c( "male", "female" ), 150, T, prob = c( .4, .6 ) ),
			note = note.of.freq( rnorm( 150, 200, 15 ) - 20 * c( 0, 1 )[ match( s, c( "male", "female" ) ) ] ),
			spl  = abs( rnorm( 150, 10, 15 ) + 3 * c( 0, 1 )[ match( s, c( "male", "female" ) ) ] ) ),
		by = "note" )

tbl <- 
	as.data.frame( table( study$note, study$sex ) )

names( tbl )[ 1 : 2 ] <-
	c( "note", "sex" )

tbl <-
	tbl[ tbl$Freq > 0, ]

ggsubplot(
	ggplot( the.piano ) +
		geom_histogram(
			aes(
				note,
				freq.of.key( key.of.note( note ) ),
				fill = color ),
			stat = "identity",
			alpha = .1 ) +
		geom_text( 
			aes( 
				x = note, 
				y = freq.of.key( key.of.note( note ) ), 
				label = paste0( note, " - ", round( frequency, 2 ), "Hz" ),
				col = note ), 
			angle = 90,
			nudge_y = 500 ) +
		#scale_fill_manual( values = c( "#000000", "#f0f3f4" ), guide = F ) +
		scale_fill_manual( values = c( "#000000", "#f0f3f4" ), guide = F ) +
		scale_color_discrete( guide = F ) +
		theme_solid( fill = "#405060" ) +
		theme( 
			axis.text.x = element_blank( ),
			axis.title.x = element_blank( ),
			axis.text.y = element_blank( ),
			axis.title.y = element_blank( ) ),
	# ggplot( piano ) +
	# 	geom_point( 
	# 		aes( 
	# 			key, 
	# 			note.of.key( key ),
	# 			col = color ) ) +
	# 	scale_color_manual( values = c( "#202031", "#e0d0c0" ) ) +
	# 	theme_dark( ),
	# ggplot( piano ) +
	# 	geom_histogram( 
	# 		aes( 
	# 			key,
	# 			note,
	# 			fill = color ),
	# 		stat = "identity" ) +
	# 	scale_fill_manual( values = c( "#201031", "#f0e8e0" ) ) +
	# 	scale_y_discrete( breaks = as.character( piano$note ), labels = piano$note ) +
	# 	theme_dark( ),
	# ggplot( piano ) +
	# 	geom_histogram( 
	# 		aes( 
	# 			key,
	# 			1,
	# 			fill = color ),
	# 		stat = "identity" ) +
	# 	scale_fill_manual( values = c( "#201031", "#f0f3f4" ) ) +
	# 	theme_dark( ),
	ggplot( the.piano ) +
		geom_histogram( 
			aes( 
				note,
				-c( 1, .63 )[ match( the.piano$color, c( "ivory", "ebony" ) ) ],
				fill = color ),
			stat = "identity" ) +
		scale_fill_manual( values = c( "#000000", "#f0f3f4" ), guide = F ) +
		geom_text( aes( note, label = note, col = color ), y = -.2, angle = 90 ) +
		scale_color_manual( values = c( "#f0f3f4", "#000000" ), guide = F ) +
		theme_solid( fill = "#405060" ) +
		theme( 
			axis.text.x = element_blank( ),
			axis.title.x = element_blank( ),
			axis.text.y = element_blank( ),
			axis.title.y = element_blank( ) ) +
		ylim( -1, 0 ),
	ggplot( the.piano[ c( 1 : 36 ), ] ) +
		geom_histogram( 
			aes( 
				1.05 ** key,
				-c( 1, .63 )[ match( the.piano[ c( 1 : 36 ), ]$color, c( "ivory", "ebony" ) ) ],
				fill = color ),
			stat = "identity", 
			alpha = 1 ) +
		scale_fill_manual( values = c( "#4f4040", "#a0a3a4" ), guide = F ) +
		geom_text( aes( 1.05 ** key, label = note, col = note ), y = -.2, angle = 90, size = 3 ) +
#		scale_color_manual( values = c( "#f0f3f4", "#000000" ), guide = F ) +
		theme_solid( fill = "#405060" ) +
		theme( 
			axis.text.x = element_blank( ),
			axis.title.x = element_blank( ),
			axis.text.y = element_blank( ),
			axis.title.y = element_blank( ) ) +
		scale_color_discrete( guide = F ) +
#		ylim( -1, 0 ) +
		geom_line( aes( 1.05 ** key, +frequency / max( frequency ), alpha = -frequency, size = frequency ), col = "#FF0000" ) +
		geom_point( aes( 1.05 ** key, +frequency / max( frequency ), col = note, size = frequency ) ) +
		scale_size( guide = F ) +
		scale_alpha( guide = F ) +
		scale_x_continuous( ),
	ggplot( 
		study ) +
		scale_color_manual( values = c( "#f0f3f4", "#000000" ) ) + 
		scale_fill_manual( values = c( "#000000", "#f0f3f4" ) ) + 
		geom_histogram( 
			aes( 
				note,
				color = color,
				fill = color ),
			stat = "count",
			position = "identity" ) +
		geom_boxplot( 
			aes( 
				note, 
				spl,
				color = color,
				fill = color ),
			width = .3,
			alpha = .93 ) +
		#geom_text( aes( note, label = note ), col = "#908070", y = +2.5, angle = 0, check_overlap = T ) +
		facet_grid( ~ sex ) +
		theme_dark( ) +
		theme( 
			plot.background = element_rect( fill = "#405060", colour = "#ff6020" ),
			panel.background = element_rect( fill = "#405060", colour = "#ff6020" ) ),
	ggplot( 
		study ) +
		geom_boxplot( aes( note, spl, col = color, fill = color ) ) +
		scale_color_manual( values = c( "#000000", "#f0f3f4" ), guide = F ) + 
		scale_fill_manual( values = c( "#f0f3f4", "#000000" ), guide = F ) + 
		# geom_histogram( 
		# 	aes( 
		# 		note,
		# 		- 5. * c( 1, .63 )[ match( color, c( "ivory", "ebony" ) ) ],
		# 		fill = color ),
		# 	stat = "identity",
		# 	position = "identity" ) +
		#scale_fill_manual( values = c( "#000000", "#f0f3f4" ), guide = F ) + 
		#geom_text( aes( note, label = note ), col = "#908070", y = +2.5, angle = 0, check_overlap = T ) +
		facet_grid( sex ~ . ) +
		#scale_color_discrete( guide = F ) +
		theme_solid( ),
	ggplot( study ) +
		geom_histogram( 
			aes( 
				note,
				fill = color ),
			stat = "count" ) +
		scale_fill_manual( values = c( "#000000", "#f0f3f4" ), guide = F ) +
		geom_text( aes( note, label = note, col = color ), y = 10, angle = 90, check_overlap = T ) +
		scale_color_manual( values = c( "#f0f3f4", "#000000" ), guide = F ) +
		theme_solid( fill = "#405060" ) +
		#theme_solid( ) +
		theme( 
			axis.text.x = element_blank( ),
			axis.title.x = element_blank( ),
			axis.text.y = element_blank( ),
			axis.title.y = element_blank( ) ) +
#		ylim( -1, 0 ) +
		facet_grid( sex ~ . ),
	layout = t(
		matrix(
			c(
				3, 3, 5, 5, 1,
				4, 4, 6, 6, 1,
				1, 1, 1, 1, 1,
				1, 1, 1, 1, 1,
				2, 2, 2, 2, 2 ),
			nrow = 5 ) ) )
