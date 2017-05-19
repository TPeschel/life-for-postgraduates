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


breaks_ <- seq( 6, 18, .5 )
labels_ <- breaks_[-1]

d$age.cat <-
    cut(
        d$age,
        breaks_,
        labels_ )

d.f0.means <-
    melt( 
        d %>%
        group_by( age.cat, sex ) %>%
        summarise(
            n = n( ), 
            m.f0.1 = mean( d.f0.1 ),
            #m.spl.1 = mean( d.spl.1 ),
            m.f0.2 = mean( d.f0.2 ),
            #m.spl.2 = mean( d.spl.2 ),
            m.f0.3 = mean( d.f0.3 ),
            #m.spl.3 = mean( d.spl.3 ),
            m.f0.4 = mean( d.f0.4 )
            #m.spl.4 = mean( d.spl.4 )
            ),
        c( "sex", "age.cat", "n" ) )
        
d.f0.err.low <-
    melt( 
        d %>%
        group_by( age.cat, sex ) %>%
        summarise(
            n = n( ), 
            low.err.f0.1  = mean( d.f0.1, na.rm = T ) - sd( d.f0.1, na.rm = T ),
            low.err.f0.2  = mean( d.f0.2, na.rm = T ) - sd( d.f0.2, na.rm = T ),
            low.err.f0.3  = mean( d.f0.3, na.rm = T ) - sd( d.f0.3, na.rm = T ),
            low.err.f0.4  = mean( d.f0.4, na.rm = T ) - sd( d.f0.4, na.rm = T )
            ),
        c( "sex", "age.cat", "n" ) )

d.f0.err.high <-
    melt( 
        d %>%
        group_by( age.cat, sex ) %>%
        summarise(
            n = n( ), 
            high.err.f0.1 = mean( d.f0.1, na.rm = T ) + sd( d.f0.1, na.rm = T ),
            high.err.f0.2 = mean( d.f0.2, na.rm = T ) + sd( d.f0.2, na.rm = T ),
            high.err.f0.3 = mean( d.f0.3, na.rm = T ) + sd( d.f0.3, na.rm = T ),
            high.err.f0.4 = mean( d.f0.4, na.rm = T ) + sd( d.f0.4, na.rm = T )
            ),
        c( "sex", "age.cat", "n" ) )

ddd <- rbind( d.f0.means, rbind( d.f0.err.low, d.f0.err.high ) )

ggplot( 
    d.f0.means, 
    aes( 
        age.cat, 
        value, 
        group = variable, 
        linetype = variable ) ) +
    geom_hline( yintercept = 0 ) +
    geom_smooth( method = "loess",alpha = .1 ) +
    geom_point( aes( size = n ), alpha = .3 ) +
    geom_path( alpha = .3 ) +
    geom_text( aes( label = as.character( round( value, 1 ) ) ), col = "#4080a0", nudge_y = -1 ) +
    geom_text( aes( label = as.character( n ) ), col = "#80a0c0", nudge_y = +1 ) +
    facet_grid( variable ~ sex ) +
    scale_linetype_discrete( name = "fundamental frequency [Hz]", breaks = c( "m.f0.1", "m.f0.2", "m.f0.3", "m.f0.4" ), labels = c( "softest speaking voice", "conversational voice", "classroom voice", "shouting voice" ) ) + 
    geom_path( aes( age.cat, value, group = variable, linetype = variable ), alpha = .2 ) +
    #geom_errorbar( )
    labs( title = "age related means of monthly increase of frequencies for speech type I-IV", x = "age [y]", y = TeX( "$\\frac{\\Delta f_{0}}{\\Delta t}\\;\\left[\\frac{Hz}{month}\\right]}$") ) +
    theme_bw( )

d.m <- d[ d$sex == "male", ]
d.f <- d[ d$sex == "female", ]

ggplot( d, aes( age.cat ) ) + geom_bar( ) + facet_grid( . ~ sex )

n <- 31
w <- rep( 1 / n, n )

d.m$m.f0.1f <- stats::filter( d.m$d.f0.1, w, sides = 2 )
d.m$m.f0.2f <- stats::filter( d.m$d.f0.2, w, sides = 2 )
d.m$m.f0.3f <- stats::filter( d.m$d.f0.3, w, sides = 2 )
d.m$m.f0.4f <- stats::filter( d.m$d.f0.4, w, sides = 2 )

ggplot( arrange( d.m, age ) ) +
    geom_point( aes( age, d.f0.4 ), col = "blue" ) +
    geom_path( aes( age, m.f0.4f ), col = "red" ) +
    geom_smooth( aes( age, m.f0.4f ), col = "black" ) +
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

