library( "ggplot2" )

cols <-
	sort( colors( ) )

dc <-
	data.frame(
		x = 1 + ( c( 0 : ( length( colors( ) ) - 1 ) ) %% 9 ),
		y = 1 + ( c( 0 : ( length( colors( ) ) - 1 ) ) %/% 9 ),
		c = cols )

ggplot( dc ) +
	theme_solid( fill = "black" ) +
	geom_text( aes( x, y, label = c, color = c ), size = 4 ) +
	scale_color_manual( values = cols, guide = F )

ggplot( dc ) +
	theme_solid( fill = "white" ) +
	geom_text( aes( x, y, label = c, color = c ), size = 4 ) +
	scale_color_manual( values = cols, guide = F )

ggplot( dc ) +
	theme_solid( fill = "white" ) +
	geom_rect( aes( xmin = x - 1 / 3, xmax = x + 1 / 3,  ymin = y - 1 / 37, ymax = y + 1 / 37, color = c ), size = 4 ) +
	geom_text( aes( x, y, label = c ), size = 4 ) +
	scale_color_manual( values = cols, guide = F )
