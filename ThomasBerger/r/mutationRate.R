#load( "../results/data_sprech.Rda" )

library( latex2exp )
library( reshape2 )
library( dplyr )
library( ggplot2 )

d <-
    data.sprech[ , c( "SIC", "sex", "age", "EDAT.x", "F0_SPRECH_1", "F0_SPRECH_2", "F0_SPRECH_3", "F0_SPRECH_4", "SPL_SPRECH_1", "SPL_SPRECH_2", "SPL_SPRECH_3", "SPL_SPRECH_4" ) ]
    
d$EDAT <- d$EDAT.x
d$EDAT.x <- NULL

d %<>%
    arrange( SIC, EDAT ) %>%
    group_by( SIC ) %>%
    mutate( 
        age    = age,
        d.t    = as.numeric( difftime( EDAT, lag( EDAT ), units = "d" ) ) * 12 / 365.25,
        d.f0.1 = ( F0_SPRECH_1 - lag( F0_SPRECH_1 ) ) / d.t,
        d.f0.2 = ( F0_SPRECH_2 - lag( F0_SPRECH_2 ) ) / d.t,
        d.f0.3 = ( F0_SPRECH_3 - lag( F0_SPRECH_3 ) ) / d.t,
        d.f0.4 = ( F0_SPRECH_4 - lag( F0_SPRECH_4 ) ) / d.t,
        d.spl.1 = ( SPL_SPRECH_1 - lag( SPL_SPRECH_1 ) ) / d.t,
        d.spl.2 = ( SPL_SPRECH_2 - lag( SPL_SPRECH_2 ) ) / d.t,
        d.spl.3 = ( SPL_SPRECH_3 - lag( SPL_SPRECH_3 ) ) / d.t,
        d.spl.4 = ( SPL_SPRECH_4 - lag( SPL_SPRECH_4 ) ) / d.t )

# d$F0_SPRECH_1 <- NULL
# d$F0_SPRECH_2 <- NULL
# d$F0_SPRECH_3 <- NULL
# d$F0_SPRECH_4 <- NULL
# d$SPL_SPRECH_1 <- NULL
# d$SPL_SPRECH_2 <- NULL
# d$SPL_SPRECH_3 <- NULL
# d$SPL_SPRECH_4 <- NULL
# d$EDAT <- NULL

d <- 
    d[ , c( "SIC", "sex", "age", "d.t", "d.f0.1", "d.f0.2", "d.f0.3", "d.f0.4" ) ]
#    d[ , c( "SIC", "sex", "age", "d.t", "d.f0.1", "d.f0.2", "d.f0.3", "d.f0.4", "d.spl.1",  "d.spl.2",  "d.spl.3",  "d.spl.4" ) ]

d <- 
    na.omit( d )


breaks_ <- seq( 6, 18, 1 )
labels_ <- breaks_[-1]

d$age.cat <-
    cut(
        d$age,
        breaks_,
        labels_ )

d.s <-
    melt( 
        d %>%
        group_by( age.cat, sex ) %>%
        summarise(
            n = n( ), 
            m.f0.1 = mean( d.f0.1 ),
            #sd.f0.1 = sd( d.f0.1 ),
            m.f0.2 = mean( d.f0.2 ),
            #sd.f0.2 = sd( d.f0.2 ),
            m.f0.3 = mean( d.f0.3 ),
            #sd.f0.3 = sd( d.f0.3 ),
            m.f0.4 = mean( d.f0.4 )#,
            #sd.f0.4 = sd( d.f0.4 )
            ),
        c( "sex", "age.cat", "n" ) )
        

ggplot( d.s, aes( age.cat, value, group = variable, linetype = variable ) ) +
    geom_hline( yintercept = 0 ) +
    geom_smooth( method = "loess",alpha = .1 ) +
    geom_point( aes( size = n ), alpha = .3 ) +
    geom_point( aes(  ),size = .1, alpha = .3 ) +
    geom_path( alpha = .3 ) +
    geom_text( aes( label = as.character( round( value, 1) ) ), col = "#4080ff", nudge_y = 1 ) +
    facet_grid( variable ~ sex ) +
    geom_path( aes( age.cat, value, group = variable, linetype = variable ), alpha = .2 ) +
    labs( title = "age related means of monthly increase of frequencies for type speech I-IV", x = "age [y]", y = TeX( "$\\frac{\\Delta f_{0}}{\\Delta t}\\;\\left[\\frac{Hz}{month}\\right]}$") ) +
    theme_bw( )

d.m <- d[ d$sex == "male", ]
d.f <- d[ d$sex == "female", ]

ggplot( d, aes( age.cat ) ) + geom_bar( ) + facet_grid( . ~ sex )

n <- 151
w <- rep( 1 / n, n )

d.m$m.f0.1f <- stats::filter( d.m$d.f0.1, w, sides = 2 )
d.m$m.f0.2f <- stats::filter( d.m$d.f0.2, w, sides = 2 )
d.m$m.f0.3f <- stats::filter( d.m$d.f0.3, w, sides = 2 )
d.m$m.f0.4f <- stats::filter( d.m$d.f0.4, w, sides = 2 )

ggplot( d.m ) +
    geom_point( aes( age - .5, d.f0.4 ), col = "blue" ) +
    geom_point( aes( age - .5, m.f0.4f ), col = "red" ) +
    #geom_smooth( aes( age - .5, m.f0.4f ), col = "black" ) +
#    geom_smooth( ) +
    facet_grid( . ~ sex )

d.unmelt$f01 <-
    stats::filter( d.unmelt$d.f0.1, w, sides = 2 )

d.unmelt$f02 <-
    stats::filter( d.unmelt$d.f0.2, w, sides = 2 )

d.unmelt$f03 <-
    stats::filter( d.unmelt$d.f0.3, w, sides = 2 )

d.unmelt$f04 <-
    stats::filter( d.unmelt$d.f0.4, w, sides = 2 )

d <-
    melt(
        data = as.data.frame( d.unmelt ),
        c( "SIC", "age", "sex", "d.t" ) )

ggplot( d, aes( age - .5, value ) ) +
    geom_line( aes( col = sex ) ) +
#    geom_smooth( ) +
    facet_grid( variable ~ sex )

