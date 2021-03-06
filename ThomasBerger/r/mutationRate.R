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

d.f0.means <-
    melt( 
        d %>%
        group_by( age.cat, sex ) %>%
        summarise(
            n = n( ), 
            speech.I = mean( d.f0.1 ),
            speech.II = mean( d.f0.2 ),
            speech.III = mean( d.f0.3 ),
            speech.IV = mean( d.f0.4 )
            ),
        c( "sex", "age.cat", "n" ) )
        
# d.spl.means <-
#     melt( 
#         d %>%
#         group_by( age.cat, sex ) %>%
#         summarise(
#             n = n( ), 
#             speech.I   = mean( d.spl.1 ),
#             speech.II  = mean( d.spl.2 ),
#             speech.III = mean( d.spl.3 ),
#             speech.IV  = mean( d.spl.4 )
#             ),
#         c( "sex", "age.cat", "n" ) )
        
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
    scale_linetype_discrete( name = "voice / speech type", breaks = c( "speech.I", "speech.II", "speech.III", "speech.IV" ), labels = c( "I softest speaking voice", "II conversational voice", "III classroom voice", " IV shouting voice" ) ) +
    scale_size_continuous( name = "count of measurements" ) +
    geom_path( aes( age.cat, value, group = variable, linetype = variable ), alpha = .2 ) +
    #geom_errorbar( )
    labs( title = "age related means of monthly increase of frequencies for speech type I-IV", x = "age [y]", y = TeX( "$\\frac{\\Delta f_{0}}{\\Delta t}\\;\\left[\\frac{Hz}{month}\\right]}$") ) +
    theme_bw( ) 
        

