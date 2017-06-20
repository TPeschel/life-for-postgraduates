rm( list = ls( ) )

library( ggplot2 )
library( ggthemes )
library( life.helper )

my.df.sex <-
	data.frame(
		pitch = c( 
			233, 204, 242, 230, 210,
			130, 112, 142, 100, 80,
			 80,  72, 112, 80,  60,
			101, 202, 303, 150, 250 ), 
		sex   = c(
			rep( "female", 5 ),
			rep( "male", 5 ),
			rep( "animal", 5 ),
			rep( "thing", 5 ) ) )

mark <-
	sample( c( 1 : 6 ), 1000, T, prob = runif( 6, 0, 1 ) )

beta <-
	runif( 6, -1, 1 )

pitch <-
	100 + beta[ mark[ 1 : 1000 ] ] * c( 1 : 1000 ) + rnorm( 1000, 0, 100 )

my.df.mark <-
	data.frame(
		pitch = pitch,
		note = as.factor( mark ) )

( xmdl.sex <-
		lm( 
			data = my.df.sex, 
			formula = pitch ~ sex ) )

summary( xmdl.sex )

( xmdl.mark <-
		lm( 
			data = my.df.mark, 
			formula = pitch ~ note ) )

summary( xmdl.mark )

lm.y.1factor <-
	function( y, fctr ) {
		library( dplyr )
		library( RColorBrewer )
		library( ggthemes )
		#y <- my.df.sex$pitch
		#fctr <- my.df.sex$sex
		loc.theme <-
			theme_hc( )
		d<-
			data.frame( 
				Y = y,
				G = fctr )
		s <-
			d %>%
				group_by( G ) %>%
					summarise( 
						Y.MEAN = mean( Y ),
						Y.VAR  = var( Y ),
						Y.SD   = sd( Y ),
						Y.SR   = sum( Y - Y.MEAN ),
						Y.SQR  = sum( ( Y - Y.MEAN ) ** 2 ),
						DF     = Y.SQR / Y.VAR ) %>%
					arrange( Y.MEAN )
		
		s$RANK.ADJ <-
			( s$Y.MEAN - min( s$Y.MEAN ) ) / ( max( s$Y.MEAN ) - min( s$Y.MEAN ) )
		
		s$RANK <-
			1 + s$RANK.ADJ * ( length( levels( s$G ) ) - 1 )
		
		s$Y.DM <-
			s$Y.MEAN - s$Y.MEAN[ 1 ]
		
		cols1 <-
			#brewer.pal( nrow( s ), "Dark2" )
			#colnames(1:10) [ 1 : nrow( ) ]
			rgb( runif( nrow( s ), 0, 1 ), runif( nrow( s ), 0, 1 ), runif( nrow( s ), 0, 1 ) )
		
		s <-
			as.data.frame( s )
		
		s.tmp <-
			s
		
		p1 <-
			ggplot( s.tmp ) +
				geom_point( aes( G, Y.MEAN, col = G ) ) +
				geom_point( inherit.aes = F, data = d, aes( G, Y, col = G ), position = "jitter", alpha = .2 ) +
			#			geom_text( aes( G, Y.MEAN, label = paste0( G, "\n", Y.MEAN ) ), nudge_x = +0, nudge_y = -10 ) +
				scale_color_manual( values = cols1 ) +
				geom_errorbar( aes( x = G, ymin = Y.MEAN - Y.SD, ymax = Y.MEAN + Y.SD, col = G ), width = .2 ) +
				#scale_x_continuous( name = "grp", breaks = s$G, labels = s$G ) +
				geom_line( aes( G, Y.MEAN ), group = "no group", alpha = .5 ) +
				loc.theme +
				theme( 
					axis.text.x  = element_text( angle = 45, vjust = 0 ),
					axis.text.y  = element_text( angle = 45 ),
					axis.title.y = element_text( angle = 90, vjust = .5 ) )
		
		s$G <-
			factor( 
				x = as.character( s$G ),
				labels = as.character( s$G ),
				levels = as.character( s$G ) )
		cols2 <-
			cols1[ match( s$G, levels( s.tmp$G ) ) ]
		
		p2 <-
			ggplot( s.tmp ) +
			geom_point( aes( G, Y.MEAN, col = G ) ) +
			geom_point( inherit.aes = F, data = d, aes( G, Y, col = G ), position = "jitter", alpha = .2 ) +
			#			geom_text( aes( G, Y.MEAN, label = paste0( G, "\n", Y.MEAN ) ), nudge_x = +0, nudge_y = -10 ) +
			scale_color_manual( values = cols1 ) +
			geom_errorbar( aes( x = G, ymin = Y.MEAN - Y.SD, ymax = Y.MEAN + Y.SD, col = G ), width = .2 ) +
			#scale_x_continuous( name = "grp", breaks = s$G, labels = s$G ) +
			geom_line( arrow = arrow( ), aes( s$G, Y.MEAN ), group = "no group", alpha = .5 ) +
			loc.theme +
			theme( 
				axis.text.x  = element_text( angle = 45, vjust = 0 ),
				axis.text.y  = element_text( angle = 45 ),
				axis.title.y = element_text( angle = 90, vjust = .5 ) )
		p3 <-
			ggplot( as.data.frame( s ) ) +
				geom_point( aes( RANK.ADJ, Y.MEAN, col = G ) ) +
			#				geom_text( aes( RANK.ADJ, Y.MEAN, label = paste0( G, "\n", Y.MEAN ) ), check_overlap = T, nudge_x = +0, nudge_y = -20 ) +
				geom_errorbar( aes( x = RANK.ADJ, ymin = Y.MEAN - Y.SD, ymax = Y.MEAN + Y.SD, col = G ), width = .02 ) +
				scale_x_continuous( breaks = round( s$RANK.ADJ, 3 ), sec.axis = sec_axis( ~ ., name = "new levels", breaks = round( s$RANK.ADJ, 3 ), labels = round( 1 + max( s$RANK ) - s$RANK, 3 ) ) ) +
				scale_y_continuous( breaks = round( s$Y.MEAN, 4 ) ) +
				scale_color_manual( values = cols2 ) +
				geom_abline( intercept = s$Y.MEAN[ 1 ], slope = s$Y.DM[ length( s$Y.DM ) ], alpha = .5 ) +
				loc.theme +
				theme( 
					axis.text.x  = element_text( angle = 45, vjust = 0.5 ),
					axis.text.y  = element_text( angle = 45 ),
					axis.title.y = element_text( angle = 90, hjust = .5, vjust = 1 ) )
			
		p4 <-
			ggplot( s ) +
				geom_point( aes( G, Y.MEAN, col = G ) ) +
				geom_point( inherit.aes = F, data = d, aes( G, Y, col = G ), position = "jitter", alpha = .2 ) +
			#			geom_text( aes( G, Y.MEAN, label = paste0( G, "\n", Y.MEAN ) ), nudge_x = +0, nudge_y = -10 ) +
				geom_errorbar( aes( x = G, ymin = Y.MEAN - Y.SD, ymax = Y.MEAN + Y.SD, col = G ), width = .2 ) +
				#scale_x_continuous( name = "grp", breaks = s$G, labels = s$G ) +
				geom_path( arrow = arrow( ), aes( G, Y.MEAN ), group = "no group", alpha = .5 ) +
				scale_color_manual( values = cols2 ) +
				loc.theme +
				theme( 
					axis.text.x  = element_text( angle = 45, vjust = 0 ),
					axis.text.y  = element_text( angle = 45 ),
					axis.title.y = element_text( angle = 90, vjust = .5 ) )
			ggsubplot( p1, p2, p3, p4, cols = 2 )
	}

lm.y.1factor( my.df.sex$pitch, my.df.sex$sex )
lm.y.1factor( my.df.mark$pitch, my.df.mark$note )

