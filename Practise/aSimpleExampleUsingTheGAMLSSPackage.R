rm( list = ls( ) )

library( hlpr4life )

load.pkgs( c( "gamlss", "gamlss.data", "ggplot2", "ggthemes" ) )

d <-
	film90[ , c( "lboopen", "lborev1") ]

m0 <-
	gamlss(
		lborev1 ~ lboopen,
		data = d )

plot( lborev1~lboopen,data=d,col="lightgray",lty=4 )
lines(fitted(m0)~d$lboopen)

ggplot( d ) + theme_bw( ) +
	geom_point( aes( lboopen, lborev1 ), col = "blue" ) +
	geom_abline( intercept =m0$mu.coefficients[ 1 ], slope = m0$mu.coefficients[ 2 ] )

ggplot( d ) +
	theme_bw( ) +
	scale_color_manual( values = c( "deeppink", "deepskyblue" ) ) +
	scale_fill_manual( values = c( "deeppink", "deepskyblue" ) ) +
	geom_point( aes( lboopen, lborev1 ), col = "blue", alpha = .1 ) +
	geom_line( aes( d$lboopen, fitted( m0 ), group = 1, col = as.factor( abs( round( residuals( m0 ) ) ) ) ), size = 2 )+
	scale_color_brewer( type = "seq", palette = 3 )


